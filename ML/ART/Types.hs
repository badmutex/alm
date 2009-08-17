module ML.ART.Types ( Choice (..)
                    , LearningRate (..)
                    , Vigilance (..)
                    , INummable

                    , ARTable (..)

                    , Categories
                    , getCategory
                    , Position
                    , Positional
                    , Output (..)
                    , fromOutput

                    , ART (..)

                    ) where

import ML.Types

import Data.Traversable
import Data.Map

newtype Choice a       = Alpha a deriving (Eq, Ord, Read, Show)
newtype LearningRate a = Beta  a deriving (Eq, Ord, Read, Show)
newtype Vigilance a    = Rho   a deriving (Eq, Ord, Read, Show)

class INummable a where

class ARTable m where
    normalize     :: Traversable t => t m -> t m
    choice        :: Choice Double
                  -> m                       -- ^ category
                  -> m                       -- ^ input datum
                  -> Double
    vigilanceTest :: Vigilance Double
                  -> m                       -- ^ category
                  -> m                       -- ^ input datum
                  -> Bool
    learn         :: LearningRate Double
                  -> m                        -- ^ old category
                  -> m                        -- ^ input datum
                  -> m                        -- ^ new category


type Categories a = Map Int a
getCategory :: Position -> Categories a -> a
getCategory k cs = cs ! k

type Position = Int
type Positional a = (Int, a)
data Output a = NewCategory (Positional a) | LearnCategory (Positional a)
fromOutput :: Output a -> Positional a
fromOutput (NewCategory   p) = p
fromOutput (LearnCategory p) = p


data ART a = ART {
      categories :: Categories a
    , output     :: Output a

    , alpha      :: Choice Double
    , beta       :: LearningRate Double
    , rho        :: Vigilance Double
    }
