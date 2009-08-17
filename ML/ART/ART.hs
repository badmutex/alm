module ML.ART.ART ( train
                  , predict
                  , module ML.ART.Types
                  ) where

import ML.ART.Types
import ML.ART.Instances

import Data.Map (assocs, insert, keys)
import Data.List (sortBy)


newIndex :: Categories a -> Position
newIndex = (+1) . last . keys

appendCategory_Map cs k c = insert k c cs

appendCategory = appendCategory_Map

sortCategories :: ARTable a => Choice Double -> Categories a -> a -> [Positional a]
sortCategories c cs d = reverse $ sortBy
                        (\(k1,v1) (k2,v2) -> compare (choice c d v1) (choice c d v2))
                        (assocs cs)

createCategory :: ARTable a => Categories a -> Int -> a -> Categories a
createCategory = appendCategory



findCategory :: ARTable a =>
                Vigilance Double -> Choice Double -> Categories a -> a -> Output a
findCategory p c cs d = case filter (\(k,v) -> vigilanceTest p v d) (sortCategories c cs d) of
                          []  -> NewCategory   (newIndex cs, d)
                          out -> LearnCategory (head out)

learnDatum :: ARTable a =>
              LearningRate Double -> Categories a -> Output a -> a -> Categories a
learnDatum lr cs (LearnCategory (k, c)) d = insert k (learn lr c d) cs
learnDatum _  cs (NewCategory   (k, c)) _ = createCategory cs k c


train :: ARTable a => ART a -> a -> ART a
train art d = let a   = alpha art
                  b   = beta art
                  p   = rho art
                  cs  = categories art
                  cat = findCategory p a cs d
              in art { categories = learnDatum b cs cat d
                     , output     = cat                  }

predict :: ARTable a => ART a -> a -> Output a
predict art d = findCategory (rho art) (alpha art) (categories art) d