{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
import qualified Control.Monad.Error as MTL
#else
import qualified Control.Monad.Except as MTL
#endif

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Monad.Freer.State
import Control.Monad.Freer.StateRW

import Criterion
import Criterion.Main
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Free as Free

--------------------------------------------------------------------------------
                        -- State Benchmarks --
--------------------------------------------------------------------------------
oneGet :: Int -> (Int, Int)
oneGet n = run (runState get n)

countDown :: Int -> (Int,Int)
countDown start = run (runState go start)
  where go = get >>= (\n -> if n <= 0 then return n else put (n-1) >> go)

countDownRW :: Int -> (Int,Int)
countDownRW start = run (runStateR go start)
  where go = ask >>= (\n -> if n <= 0 then return n else tell (n-1) >> go)

countDownMTL :: Int -> (Int,Int)
countDownMTL = MTL.runState go
  where go = MTL.get >>= (\n -> if n <= 0 then return n else MTL.put (n-1) >> go)

--------------------------------------------------------------------------------
                       -- Exception + State --
--------------------------------------------------------------------------------
countDownExc :: Int -> Either String (Int,Int)
countDownExc start = run $ runError (runState go start)
  where go = get >>= (\n -> if n <= (0 :: Int) then throwError "wat" else put (n-1) >> go)

countDownExcMTL :: Int -> Either String (Int,Int)
countDownExcMTL = MTL.runStateT go
  where go = MTL.get >>= (\n -> if n <= (0 :: Int) then MTL.throwError "wat" else MTL.put (n-1) >> go)

--------------------------------------------------------------------------------
                          -- Freer: Interpreter --
--------------------------------------------------------------------------------
data Http out where
  Open :: String -> Http ()
  Close :: Http ()
  Post  :: String -> Http String
  Get   :: Http String

open' :: Member Http r => String -> Eff r ()
open'  = send . Open

close' :: Member Http r => Eff r ()
close' = send Close

post' :: Member Http r => String -> Eff r String
post' = send . Post

get' :: Member Http r => Eff r String
get' = send Get

runHttp :: Eff (Http ': r) w -> Eff r w
runHttp (Val x) = return x
runHttp (E u q) = case decomp u of
  Right (Open _) -> runHttp (qApp q ())
  Right Close    -> runHttp (qApp q ())
  Right (Post d) -> runHttp (qApp q d)
  Right Get      -> runHttp (qApp q "")
  Left u'        -> E u' (tsingleton (runHttp . qApp q ))

--------------------------------------------------------------------------------
                          -- Free: Interpreter --
--------------------------------------------------------------------------------
data FHttpT x
  = FOpen String x
  | FClose x
  | FPost String (String -> x)
  | FGet (String -> x)
    deriving Functor

type FHttp = Free.Free FHttpT

fopen' :: String -> FHttp ()
fopen' s = Free.liftF $ FOpen s ()

fclose' :: FHttp ()
fclose' = Free.liftF $ FClose ()

fpost' :: String -> FHttp String
fpost' s = Free.liftF $ FPost s id

fget' :: FHttp String
fget' = Free.liftF $ FGet id

runFHttp :: FHttp a -> Maybe a
runFHttp (Free.Pure x) = return x
runFHttp (Free.Free (FOpen _ n)) = runFHttp n
runFHttp (Free.Free (FClose n))  = runFHttp n
runFHttp (Free.Free (FPost s n)) = pure s  >>= runFHttp . n
runFHttp (Free.Free (FGet n))    = pure "" >>= runFHttp . n

--------------------------------------------------------------------------------
                        -- Benchmark Suite --
--------------------------------------------------------------------------------
prog :: Member Http r => Eff r ()
prog = open' "cats" >> get' >> post' "cats" >> close'

prog' :: FHttp ()
prog' = fopen' "cats" >> fget' >> fpost' "cats" >> fclose'

p :: Member Http r => Int -> Eff r ()
p count   =  open' "cats" >> replicateM_ count (get' >> post' "cats") >>  close'

p' :: Int -> FHttp ()
p' count  = fopen' "cats" >> replicateM_ count (fget' >> fpost' "cats") >> fclose'

main :: IO ()
main =
  defaultMain [
    bgroup "State" [
        bench "get"          $ whnf oneGet 0
    ],
    bgroup "Countdown Bench" [
        bench "freer.State"    $ whnf countDown 10000
      , bench "freer.StateRW"  $ whnf countDownRW 10000
      , bench "mtl.State"      $ whnf countDownMTL 10000
    ],
    bgroup "Countdown+Except Bench" [
        bench "freer.ExcState"  $ whnf countDownExc 10000
      , bench "mtl.ExceptState" $ whnf countDownExcMTL 10000
    ],
    bgroup "HTTP Simple DSL" [
        bench "freer" $ whnf (run . runHttp) prog
      , bench "free" $ whnf runFHttp prog'

      , bench "freerN"      $ whnf (run . runHttp . p) 100000
      , bench "freeN"       $ whnf (runFHttp . p')     100000
    ]
  ]
