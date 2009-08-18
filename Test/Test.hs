{-# LANGUAGE NoMonomorphismRestriction #-}

module Test.Test where

import Control.Monad
import Control.Applicative ((<$>))
import System.IO.Unsafe
import System.Random
import System.Random.Shuffle (shuffle')
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L

import qualified ML.ART.ART as ART
import qualified ML.ARTMap.ARTMap as AMAP
import ML.Internal.Types.Vector
import ML.Internal.Types
import ML.Stats


readIrisData :: FilePath -> IO [[Double]]
readIrisData p = do
  ls <- (fmap (BS.split ',') . BS.split '\n') <$> BS.readFile p
  let ls' = map (\l -> splitAt (length l - 1) l) $(fmap . fmap) (BS.unpack) ls
      vs = map (map (read) . fst) ls'
  return $ L.filter (not . L.null) vs

readIrisData' :: FilePath -> IO [Double]
readIrisData' p = do
  ls <- (fmap (BS.split ',') . BS.split '\n') <$> BS.readFile p
  let ls' = map (\l -> splitAt (length l - 1) l) $(fmap . fmap) (BS.unpack) ls
      vs = map snd ls'
  return $ map (cat . head) $ L.filter (not . L.null) vs

cat "Iris-setosa" = 0
cat "Iris-versicolor" = 50
cat "Iris-virginica" = 100

input = unsafePerformIO . readIrisData
output = map (\c -> [c]) . unsafePerformIO . readIrisData'


r = unsafePerformIO $ readIrisData' $ "/Users/badi/src/alm.git/Test/iris.data"

info home = unsafePerformIO . readIrisData $ "/" ++ home ++ "/badi/src/alm.git/Test/iris.data"

showInfo = mapM_ (putStrLn . show) $ info "Users"

runART art p home = 
    let d = ART.normalize $ info home
    in length . M.toList . ART.categories $  foldl ART.train (art {ART.rho = ART.Rho p}) d


arta = ART.ART { ART.categories = M.empty
               , ART.alpha      = ART.Alpha 0.01
               , ART.beta       = ART.Beta 1
               , ART.rho        = ART.Rho 0.5
               , ART.output     = undefined
               }

artb = arta

path = "/badi/src/alm.git/Test/iris.data"
training_data = unsafePerformIO $ do g <- getStdRandom split
                                     let td' = shuffle' td (length td) g
                                     return $ splitAt (floor ((fromIntegral $ length td') * 0.8)) td'
    where norm_p = ART.normalize $ input ("/Users" ++ path)
          norm_t = ART.normalize $ output ("/Users" ++ path)
          td = map (\(p, t) -> AMAP.TD p t) $ zip norm_p norm_t

runARTMap amap = foldl test [] test_data
    where td = fst training_data
          test_data = snd training_data
          amap' = foldl AMAP.train amap td
          test rs d = let output = AMAP.predict amap' $ AMAP.pattern d
                      in AvP "" (AMAP.target d) output : rs

distances = distance $ runARTMap amap
ds = distances
          

amap = AMAP.ARTMap { AMAP.arta     = arta
                   , AMAP.artb     = artb
                   , AMAP.mapfield = M.empty
                   , AMAP.output   = undefined
                   }


show' :: Show a => [a] -> IO ()
show' = mapM_ (putStrLn . show)
