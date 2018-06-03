{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module:       Control.Monad.Freer
Description:  Freer - an extensible effects library
Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
License:      BSD3
Maintainer:   Alexis King <lexi.lambda@gmail.com>
Stability:    experimental
Portability:  GHC specific language extensions.

This library is an implementation of an /extensible effect system/ for Haskell,
a general-purpose way of tracking effects at the type level and handling them in
different ways. The concept of an “effect” is very general: it encompasses the
things most people consider side-effects, like generating random values,
interacting with the file system, and mutating state, but it also includes
things like access to an immutable global environment and exception handling.

Traditional Haskell tracks and composes effects using /monad transformers/,
which involves modeling each effects using what is conceptually a separate
monad. In contrast, @freer-simple@ provides exactly __one__ monad, 'Eff',
parameterized by a type-level list of effects. For example, a computation that
produces an 'Integer' by consuming a 'String' from the global environment and
acting upon a single mutable cell containing a 'Bool' would have the following
type:

@
'Eff' '['Control.Monad.Freer.Reader.Reader' 'String', 'Control.Monad.Freer.State.State' 'Bool'] 'Integer'
@

For comparison, this is the equivalent stack of monad transformers:

@
ReaderT 'String' (State 'Bool') 'Integer'
@

However, this is slightly misleading: the example with 'Eff' is actually
/more general/ than the corresponding example using transformers because the
implementations of effects are not /concrete/. While @StateT@ specifies a
/specific/ implementation of a pseudo-mutable cell,
'Control.Monad.Freer.State.State' is merely an interface with a set of available
operations. Using 'Control.Monad.Freer.State.runState' will “run” the
'Control.Monad.Freer.State.State' effect much the same way that @StateT@ does,
but a hypothetical handler function @runStateTVar@ could implement the state in
terms of a STM 'Control.Concurrent.STM.TVar'.

The @freer-simple@ effect library is divided into three parts:

  1. First, @freer-simple@ provides the 'Eff' monad, an implementation of
     extensible effects that allows effects to be tracked at the type level and
     interleaved at runtime.

  2. Second, it provides a built-in library of common effects, such as
     'Control.Monad.Freer.Reader.Reader', 'Control.Monad.Freer.Writer.Writer',
     'Control.Monad.Freer.State.State', and 'Control.Monad.Freer.Error.Error'.
     These effects can be used with 'Eff' out of the box with an interface that
     is similar to the equivalent monad transformers.

  3. Third, it provides a set of combinators for implementing your /own/
     effects, which can either be implemented entirely independently, in terms
     of other existing effects, or even in terms of existing monads, making it
     possible to use @freer-simple@ with existing monad transformer stacks.

One of the core ideas of @freer-simple@ is that /most/ effects that occur in
practical applications are really different incarnations of a small set of
fundamental effect types. Therefore, while it’s possible to write new effect
handlers entirely from scratch, it’s more common that you will wish to define
new effects in terms of other effects. @freer-simple@ makes this possible by
providing the 'reinterpret' function, which allows /translating/ an effect into
another one.

For example, imagine an effect that represents interactions with a file system:

@
data FileSystem r where
  ReadFile :: 'FilePath' -> FileSystem 'String'
  WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
@

An implementation that uses the real file system would, of course, be
implemented in terms of 'IO'. An alternate implementation, however, might be
implemented in-memory in terms of 'Control.Monad.Freer.State.State'. With
'reinterpret', this implementation is trivial:

@
runInMemoryFileSystem :: [('FilePath', 'String')] -> 'Eff' (FileSystem ': effs) '~>' 'Eff' effs
runInMemoryFileSystem initVfs = 'Control.Monad.Freer.State.evalState' initVfs '.' fsToState where
  fsToState :: 'Eff' (FileSystem ': effs) '~>' 'Eff' ('Control.Monad.Freer.State.State' [('FilePath', 'String')] ': effs)
  fsToState = 'reinterpret' '$' \case
    ReadFile path -> 'Control.Monad.Freer.State.get' '>>=' \\vfs -> case 'lookup' path vfs of
      'Just' contents -> 'pure' contents
      'Nothing' -> 'error' ("readFile: no such file " ++ path)
    WriteFile path contents -> 'Control.Monad.Freer.State.modify' $ \\vfs ->
      (path, contents) : 'Data.List.delete' (path, contents) vfs
@

This handler is easy to write, doesn’t require any knowledge of how
'Control.Monad.Freer.State.State' is implemented, is entirely encapsulated, and
is composable with all other effect handlers. This idea—making it easy to define
new effects in terms of existing ones—is the concept around which @freer-simple@
is based.

= Effect Algebras

In @freer-simple@, effects are defined using /effect algebras/, which are
representations of an effect’s operations as a generalized algebraic datatype,
aka GADT. This might sound intimidating, but you really don’t need to know very
much at all about how GADTs work to use @freer-simple@; instead, you can just
learn the syntax entirely in terms of what it means for defining effects.

Consider the definition of the @FileSystem@ effect from the above example:

@
data FileSystem r where
  ReadFile :: 'FilePath' -> FileSystem 'String'
  WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
@

The first line, @data FileSystem r where@, defines a new effect. All effects
have at least one parameter, normally named @r@, which represents the /result/
or /return type/ of the operation. For example, take a look at the type of
@ReadFile@:

@
ReadFile :: 'FilePath' -> FileSystem 'String'
@

This is very similar to the type of 'readFile' from the standard "Prelude",
which has type @'FilePath' -> 'IO' 'String'@. The only difference is that the
name of the effect, in this case @FileSystem@, replaces the use of the monad,
in this case 'IO'.

Also notice that @ReadFile@ and @WriteFile@ begin with capital letters. This is
because they are actually /data constructors/. This means that
@ReadFile "foo.txt"@ actually constructs a /value/ of type
@FileSystem 'String'@, and this is useful, since it allows effect handlers like
@runInMemoryFileSystem@ to pattern-match on the effect’s constructors and get
the values out.

To actually /use/ our @FileSystem@ effect, however, we have to write just a
little bit of glue to connect our effect definition to the 'Eff' monad, which we
do using the 'send' function. We can write an ordinary function for each of the
@FileSystem@ constructors that mechanically calls 'send':

@
readFile :: 'Member' FileSystem effs => 'FilePath' -> 'Eff' effs 'String'
readFile path = 'send' (ReadFile path)

writeFile :: 'Member' FileSystem effs => 'FilePath' -> 'String' -> 'Eff' effs ()
writeFile path contents = 'send' (WriteFile path contents)
@

Notice the use of the 'Member' constraint on these functions. This constraint
means that the 'FileSystem' effect can be anywhere within the type-level list
represented by the @effs@ variable. If the signature of 'readFile' were more
concrete, like this:

@
readFile :: 'FilePath' -> 'Eff' '[FileSystem] 'String'
@

…then 'readFile' would /only/ be usable with an 'Eff' computation that /only/
performed @FileSystem@ effects, which isn’t especially useful.
-}
module Control.Monad.Freer
  ( -- * Effect Monad
    Eff

    -- ** Effect Constraints
    -- | As mentioned in the documentation for 'Eff', it’s rare to actually
    -- specify a concrete list of effects for an 'Eff' computation, since that
    -- has two significant downsides:
    --
    --   1. It couples the computation to that /specific/ list of effects, so it
    --      cannot be used in functions that perform a strict superset of
    --      effects.
    --
    --   2. It forces the effects to be handled in a particular order, which
    --      can make handler code brittle when the list of effects is changed.
    --
    -- Fortunately, these restrictions are easily avoided by using
    -- /effect constraints/, such as 'Member' or 'Members', which decouple a
    -- computation from a particular concrete list of effects.
  , Member
  , Members
  , LastMember

    -- ** Sending Arbitrary Effects
  , send
  , sendM

    -- ** Lifting Effect Stacks
  , raise

    -- * Handling Effects
    -- | Once an effectful computation has been produced, it needs to somehow be
    -- executed. This is where /effect handlers/ come in. Each effect can have
    -- an arbitrary number of different effect handlers, which can be used to
    -- interpret the same effects in different ways. For example, it is often
    -- useful to have two effect handlers: one that uses 'sendM' and
    -- 'interpretM' to interpret the effect in 'IO', and another that uses
    -- 'interpret', 'reinterpret', or 'translate' to interpret the effect in an
    -- entirely pure way for the purposes of testing.
    --
    -- This module doesn’t provide any effects or effect handlers (those are in
    -- their own modules, like "Control.Monad.Freer.Reader" and
    -- "Control.Monad.Freer.Error"), but it /does/ provide a set of combinators
    -- for constructing new effect handlers. It also provides the 'run' and
    -- 'runM' functions for extracting the actual result of an effectful
    -- computation once all effects have been handled.

    -- ** Running the Eff monad
  , run
  , runM

    -- ** Building Effect Handlers
    -- *** Basic effect handlers
  , HasLen
  , interpret
  , interpose
  , subsume
    -- *** Derived effect handlers
  , reinterpret
  , reinterpret2
  , reinterpret3
  , reinterpretN
  , translate
    -- *** Monadic effect handlers
  , interpretM
    -- *** Advanced effect handlers
  , interpretWith
  , interposeWith

    -- * Re-exported bindings
  , type (~>)
  ) where

import Control.Natural (type (~>))

import qualified Control.Monad.Freer.Internal as Internal

import Control.Monad.Freer.Internal
  ( Eff
  , LastMember
  , Member
  , HasLen
  , Members
  , Weakens
  , (:++:)
  , handleRelay
  , raise
  , replaceRelay
  , replaceRelayN
  , run
  , runM
  , send
  , sendM
  )

-- | The simplest way to produce an effect handler. Given a natural
-- transformation from some effect @eff@ to some effectful computation with
-- effects @effs@, produces a natural transformation from @'Eff' (eff ': effs)@
-- to @'Eff' effs@.
interpret :: forall eff effs. HasLen effs => (eff ~> Eff effs) -> Eff (eff ': effs) ~> Eff effs
interpret f = interpretWith (\e -> (f e >>=))
{-# INLINE interpret #-}

-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
interpose :: forall eff effs. Member eff effs => (eff ~> Eff effs) -> Eff effs ~> Eff effs
interpose f = interposeWith (\e -> (f e >>=))
{-# INLINE interpose #-}

-- | Interprets an effect in terms of another identical effect. This can be used
-- to eliminate duplicate effects.
subsume :: forall eff effs. (Member eff effs, HasLen effs) => Eff (eff ': effs) ~> Eff effs
subsume = interpret send
{-# INLINE subsume #-}

-- | Like 'interpret', but instead of removing the interpreted effect @f@,
-- reencodes it in some new effect @g@.
reinterpret :: forall f g effs. HasLen effs => (f ~> Eff (g ': effs)) -> Eff (f ': effs) ~> Eff (g ': effs)
reinterpret f = replaceRelay pure (\e -> (f e >>=))
{-# INLINE reinterpret #-}

-- | Like 'reinterpret', but encodes the @f@ effect in /two/ new effects instead
-- of just one.
reinterpret2
  :: forall f g h effs
   . HasLen effs
   => (f ~> Eff (g ': h ': effs)) -> Eff (f ': effs) ~> Eff (g ': h ': effs)
reinterpret2 = reinterpretN @[g, h]
{-# INLINE reinterpret2 #-}

-- | Like 'reinterpret', but encodes the @f@ effect in /three/ new effects
-- instead of just one.
reinterpret3
  :: forall f g h i effs
   . HasLen effs
  => (f ~> Eff (g ': h ': i ': effs))
  -> Eff (f ': effs) ~> Eff (g ': h ': i ': effs)
reinterpret3 = reinterpretN @[g, h, i]
{-# INLINE reinterpret3 #-}

-- | Like 'interpret', 'reinterpret', 'reinterpret2', and 'reinterpret3', but
-- allows the result to have any number of additional effects instead of simply
-- 0-3. The problem is that this completely breaks type inference, so you will
-- have to explicitly pick @gs@ using @TypeApplications@. Prefer 'interpret',
-- 'reinterpret', 'reinterpret2', or 'reinterpret3' where possible.
reinterpretN
  :: forall gs f effs. (Weakens gs, HasLen effs)
  => (f ~> Eff (gs :++: effs)) -> Eff (f ': effs) ~> Eff (gs :++: effs)
reinterpretN f = replaceRelayN @gs pure (\e -> (f e >>=))
{-# INLINE reinterpretN #-}

-- | Runs an effect by translating it into another effect. This is effectively a
-- more restricted form of 'reinterpret', since both produce a natural
-- transformation from @'Eff' (f ': effs)@ to @'Eff' (g ': effs)@ for some
-- effects @f@ and @g@, but 'translate' does not permit using any of the other
-- effects in the implementation of the interpreter.
--
-- In practice, this difference in functionality is not particularly useful, and
-- 'reinterpret' easily subsumes all of the functionality of 'translate', but
-- the way 'translate' restricts the result leads to much better type inference.
--
-- @
-- 'translate' f = 'reinterpret' ('send' . f)
-- @
translate :: forall f g effs. HasLen effs => (f ~> g) -> Eff (f ': effs) ~> Eff (g ': effs)
translate f = reinterpret (send . f)
{-# INLINE translate #-}

-- | Like 'interpret', this function runs an effect without introducing another
-- one. Like 'translate', this function runs an effect by translating it into
-- another effect in isolation, without access to the other effects in @effs@.
-- Unlike either of those functions, however, this runs the effect in a final
-- monad in @effs@, intended to be run with 'runM'.
--
-- @
-- 'interpretM' f = 'interpret' ('sendM' . f)
-- @
interpretM
  :: forall eff m effs
   . (Monad m, LastMember m effs, HasLen effs)
  => (eff ~> m) -> Eff (eff ': effs) ~> Eff effs
interpretM f = interpret (sendM . f)
{-# INLINE interpretM #-}

-- | A highly general way of handling an effect. Like 'interpret', but
-- explicitly passes the /continuation/, a function of type @v -> 'Eff' effs b@,
-- to the handler function. Most handlers invoke this continuation to resume the
-- computation with a particular value as the result, but some handlers may
-- return a value without resumption, effectively aborting the computation to
-- the point where the handler is invoked. This is useful for implementing
-- things like 'Control.Monad.Freer.Error.catchError', for example.
--
-- @
-- 'interpret' f = 'interpretWith' (\e -> (f e '>>='))
-- @
interpretWith
  :: forall eff effs b
   . HasLen effs
  => (forall v. eff v -> (v -> Eff effs b) -> Eff effs b)
  -> Eff (eff ': effs) b
  -> Eff effs b
interpretWith = handleRelay pure
{-# INLINE interpretWith #-}

-- | Combines the interposition behavior of 'interpose' with the
-- continuation-passing capabilities of 'interpretWith'.
--
-- @
-- 'interpose' f = 'interposeWith' (\e -> (f e '>>='))
-- @
interposeWith
  :: forall eff effs b
   . Member eff effs
  => (forall v. eff v -> (v -> Eff effs b) -> Eff effs b)
  -> Eff effs b
  -> Eff effs b
interposeWith = Internal.interpose pure
{-# INLINE interposeWith #-}
