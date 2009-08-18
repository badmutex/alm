module ML.Stats where

import ML.Internal.Types ( ActualVsPredicted (..)
                         , Metric (..)
                         )
import qualified Math.Statistics as S


pearson :: Floating a => [ActualVsPredicted a] -> a
pearson es = let as = map actual es
                 ps = map predicted es
             in S.pearson as ps

distance :: Metric a a => [ActualVsPredicted a] -> [Double]
distance = map (\avp -> actual avp <-> predicted avp)
