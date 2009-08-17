module ML.ARTMap.Types where

import ML.ART.Types (ART, Output, Position, Vigilance)

import Prelude hiding (lookup)
import Data.Map


type Mapfield a = Map Position (Position, a)

createMapping :: Position -> Position -> Mapfield a -> a -> Mapfield a
createMapping p1 p2 f d = insert p1 (p2,d) f

findMapping :: Position -> Mapfield a -> (Position, a)
findMapping p f = f ! p

data ARTMap a b = ARTMap {
      arta :: ART a
    , artb :: ART b
    , mapfield :: Mapfield Double

    , output :: Output b


    }


data TrainingDatum a b = TD { pattern :: a
                            , target  :: b }
                       deriving (Eq, Read, Show)