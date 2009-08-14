
module ML.Types.Vector where

import ML.Types.Classes

data Vector a = Vector [a] deriving (Eq, Read, Show)

instance Functor Vector where
    fmap f (Vector v) = Vector (fmap f v)

