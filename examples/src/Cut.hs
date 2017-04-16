{-# LANGUAGE NoImplicitPrelude #-}
module Cut () where

-- import Control.Monad.Freer.Cut


{-
-- The signature is inferred
tcut1 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
tcut1 = (return (1::Int) `mplus'` return 2) `mplus'`
         ((cutfalse `mplus'` return 4) `mplus'`
          return 5)

tcut1r = run . makeChoice $ call tcut1
-- [1,2]

tcut2 = return (1::Int) `mplus'`
         call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'`
               return 4)
       `mplus'` return 5

-- Here we see nested call. It poses no problems...
tcut2r = run . makeChoice $ call tcut2
-- [1,2,5]

-- More nested calls
tcut3 = call tcut1 `mplus'` call (tcut2 `mplus'` cutfalse)
tcut3r = run . makeChoice $ call tcut3
-- [1,2,1,2,5]

tcut4 = call tcut1 `mplus'`  (tcut2 `mplus'` cutfalse)
tcut4r = run . makeChoice $ call tcut4
-- [1,2,1,2,5]
-}
