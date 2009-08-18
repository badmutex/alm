{-# LANGUAGE
  MultiParamTypeClasses
  #-}

module ML.Internal.Types where

type Distance = Double
class Metric a b where
    (<->) :: a -> b -> Distance

class Merge a b c where
    (<~>) :: a -> b -> c

class Functor f => Defunctor f where
    pull :: f a -> a

class INummable a where


data ActualVsPredicted a = AvP { label :: String
                               , actual :: a
                               , predicted :: a }
