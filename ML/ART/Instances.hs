{-# LANGUAGE
  FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  #-}

module ML.ART.Instances where

import ML.ART.Types

import Control.Applicative



instance Functor Choice where
    fmap f (Alpha a) = Alpha (f a)

instance Applicative Choice where
    pure            = Alpha
    (<*>) (Alpha f) = fmap f

instance INummable (Choice a)  where


instance Functor LearningRate where
    fmap f (Beta b) = Beta (f b)

instance Applicative LearningRate where
    pure           = Beta
    (<*>) (Beta f) = fmap f

instance INummable (LearningRate b) where


instance Functor Vigilance where
    fmap f (Rho p) = Rho (f p)

instance Applicative Vigilance where
    pure          = Rho
    (<*>) (Rho f) = fmap f

instance INummable (Vigilance p) where
