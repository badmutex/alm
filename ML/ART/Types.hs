module ML.ART.Types ( Choice (..)
                    , LearningRate (..)
                    , Vigilance (..)

                    , ARTable (..)

                    , Categories
                    , getCategory
                    , Position
                    , Positional
                    , Output (..)
                    , fromOutput

                    , ART (..)

                    ) where

import ML.Internal.Types

import Data.Traversable
import Data.Map

newtype Choice a       = Alpha a deriving (Eq, Ord, Read, Show)
newtype LearningRate a = Beta  a deriving (Eq, Ord, Read, Show)
newtype Vigilance a    = Rho   a deriving (Eq, Ord, Read, Show)

-- | Any datatype that wishes to be classified using an ART module
-- should provide an instance of 'ARTable'.
class ARTable m where
    normalize     :: [m] -> [m]
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


type Categories a = Map Position a
getCategory :: Position -> Categories a -> a
getCategory k cs = cs ! k

type Position = Int

-- | Maps to the position in the ARTb category and the mapping type
type Positional a = (Position, a)

-- | Denotes that a new category should be created in order to learn
-- the datum or that it can be merged with a current category.
data Output a = NewCategory   (Positional a)
              | LearnCategory (Positional a)
              deriving (Eq, Read, Show)
fromOutput :: Output a -> Positional a
fromOutput (NewCategory   p) = p
fromOutput (LearnCategory p) = p


data ART a = ART {
      categories :: Categories a
    , output     :: Output a

    , alpha      :: Choice Double
    , beta       :: LearningRate Double
    , rho        :: Vigilance Double
    } deriving (Read, Show)
