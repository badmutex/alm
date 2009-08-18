{-# LANGUAGE
  MultiParamTypeClasses
  #-}

module ML.Internal.Types where

type Distance = Double


-- | Calculate a distance measurement between two data structure.
-- This metric should satisfy the following conditions:
--
-- 1. @x \<-\> y >= 0@ (non-negativity)
--
-- 2. @x \<-\> y == 0 iff x == y@ (identity of indescernibles)
--
-- 3. @x \<-\> y == y \<-\> x@ (symmetry)
--
-- 4. @x \<-\> z \<= x \<-\> y + y \<-\> z@ (subadditivity / triangle inequality)
--
-- See <http://en.wikipedia.org/wiki/Metric_(mathematics)> for more info.
class Metric a b where
    (<->) :: a -> b -> Distance


-- | Merge two types together to produce a third. 
-- Usually @a@, @b@, and @c@ are of the same type
class Merge a b c where
    (<~>) :: a -> b -> c

-- | Pull data out of a functor
class Functor f => Defunctor f where
    pull :: f a -> a

-- | If any type, ie 'Vigilance', needs to support numeric operations
-- it should provide an instance of 'Functor', 'Applicative', and the
-- necessary numeric classes
class INummable a where



data TrainingDatum a b = TD { id      :: String
                            , pattern :: a
                            , target  :: b }
                       deriving (Eq, Read, Show)


-- | Used for error analysis and statistics.
data ActualVsPredicted a = AvP { label     :: String
                               , actual    :: a
                               , predicted :: a 
                               } deriving (Eq, Read, Show)
