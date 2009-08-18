{-# LANGUAGE
  MultiParamTypeClasses
  , FlexibleInstances
  , NoMonomorphismRestriction
  , TypeSynonymInstances
  #-}

module ML.Internal.Types.Vector ( Vector
                                ) where

import ML.Internal.Types
import ML.ART.Types ( ARTable (..)
                    , Choice (..)
                    , LearningRate (..)
                    , Vigilance (..)
                    )

import Control.Applicative
import Data.List (transpose)


type Vector = [Double]

norm  = sqrt . sum . map (^2)

fuzzyVectorNormalize vs = 
    let minmaxes = map (\v -> (minimum v, maximum v)) $ transpose vs
    in  map (zipWith (\(min', max') x -> (max' - x) / (max' - min')) minmaxes) vs

complementEncode vs = zipWith (++) vs (map (map (1 -)) vs)

fuzzyAND = zipWith min
(/\) = fuzzyAND

(.*>) n = (<$>) (*n)
infixl 7 .*>

(<+>) = zipWith (+)
infixl 6 <+>


instance ARTable Vector where
    normalize                    = complementEncode . fuzzyVectorNormalize
    choice (Alpha a) c d         = let t = norm $ d /\ c
                                       b = a + norm c
                                   in t / b
    vigilanceTest (Rho p) c d    = let t = norm $ d /\ c
                                       b = norm d
                                   in t / b >= p
    learn (Beta b) c d = b .*> (d /\ c) <+> (1 - b) .*> c


instance Metric Vector Vector where
    xs <-> ys = sqrt . sum $ zipWith (\x y -> (x-y)^2) xs ys
