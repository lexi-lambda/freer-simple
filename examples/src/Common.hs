module Common where

import Control.Applicative

add :: Applicative f => f Int -> f Int -> f Int
add = liftA2 (+)
