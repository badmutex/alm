{-# LANGUAGE
  FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  #-}


module ML.Internal.Instances where

import ML.Internal.Types

import Control.Applicative

instance ( Applicative f
         , Num         n
         , Eq          (f n)
         , INummable   (f n)
         , Show        (f n))
    => Num (f n) where
        (+)         = liftA2 (+)
        (-)         = liftA2 (-)
        (*)         = liftA2 (*)
        abs         = liftA abs
        signum      = liftA signum
        fromInteger = liftA fromInteger . pure

instance ( Applicative f
         , Fractional  n
         , Eq          (f n)
         , INummable   (f n)
         , Show        (f n))
    => Fractional (f n) where
        (/)          = liftA2 (/)
        recip        = liftA recip
        fromRational = liftA fromRational . pure

instance ( Applicative f
         , Floating    n
         , INummable  (f n)
         , Eq         (f n)
         , Show       (f n))
    => Floating (f n) where 
        pi = pi
        exp = liftA exp
        log = liftA log
        sin = liftA sin
        cos = liftA cos
        sinh = liftA sinh
        cosh = liftA cosh
        asin = liftA asin
        acos = liftA acos
        atan = liftA atan
        asinh = liftA asinh
        atanh = liftA atanh
        acosh = liftA acosh


