{-# LANGUAGE
  MultiParamTypeClasses
  , RankNTypes
  #-}

module ML.ART.Types where

import ML.Types

import Data.Traversable
import Data.Map

newtype Choice a       = Alpha a deriving (Eq, Ord, Read, Show)
newtype LearningRate a = Beta  a deriving (Eq, Ord, Read, Show)
newtype Vigilance a    = Rho   a deriving (Eq, Ord, Read, Show)

class INummable a where

class (Merge m m m) => Matchable m where
    (<->) :: m -> m -> m -- ^ some sort of comparison. ie: @zipWith min@ for fuzzy vectors
    (.*>) :: LearningRate a -> m -> m -- ^ adjust the object by the learning rate
    (<~>) :: m -> m -> m -- ^ Merge. ie: @zipWith (+)@ for two vectors
    norm :: m -> Double -- ^ie: @|v|@ for a vector

class (Matchable m) => ARTable m where
    normalize :: Traversable t => t m -> t m
    choice :: Choice Double
                                 -> m -- ^ category
                                 -> m -- ^ input datum
                                 -> Double
    vigilanceTest :: Vigilance Double
                                 -> m -- ^ category
                                 -> m -- ^ input datum
                                 -> Bool
    learn :: LearningRate Double
                                 -> m -- ^ old category
                                 -> m -- ^ input datum
                                 -> m -- ^ new category


type Categories a = Map Int a
type Positional a = (Int, a)
data Output a = NewCategory (Positional a) | LearnCategory (Positional a)
fromOutput (NewCategory   (_, v)) = v
fromOutput (LearnCategory (_, v)) = v

data ART a = ART {
      categories :: Categories a
    , output :: Output a

    , alpha :: Choice Double
    , beta :: LearningRate Double
    , rho :: Vigilance Double
    }
