module ML.ARTMap.ARTMap ( train
                        , predict
                        , module ML.ARTMap.Types
                        ) where

import qualified ML.ART.ART as ART
import ML.ARTMap.Types


train :: (ART.ARTable a, ART.ARTable b) => ARTMap a b -> TrainingDatum a b -> ARTMap a b
train amap d = let arta'   = ART.train (arta amap) (pattern d)
                   artb'   = ART.train (artb amap) (target  d)
                   (pa, _) = ART.fromOutput $ ART.output arta'
                   (pb, _) = ART.fromOutput $ ART.output artb'
               in amap { arta     = arta'
                       , artb     = artb'
                       , mapfield = createMapping pa pb (mapfield amap) 1
                       , output   = ART.output artb'
                       }

predict :: ART.ARTable a => ARTMap a b -> a -> Maybe b
predict amap d = let (p,_)  = ART.fromOutput $ ART.predict (arta amap) d
                 in  (\(p',_) -> ART.getCategory p' (ART.categories $ artb amap))
                         `fmap`
                         findMapping p (mapfield amap)


