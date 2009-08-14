{-# LANGUAGE
  MultiParamTypeClasses
  #-}

module ML.Internal.Types where

type Distance = Double
class Metric a b where
    (<->) :: a -> b -> Distance

class Merge a b c where
    (<~>) :: a -> b -> c
