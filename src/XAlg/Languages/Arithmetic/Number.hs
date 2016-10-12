{-# LANGUAGE FlexibleInstances,
             DeriveDataTypeable #-}

module XAlg.Languages.Arithmetic.Number where

import XAlg.Foundation.Core

import Prelude hiding               (Float, Int)
import qualified  Data.Complex as C (Complex)
import Data.Complex          hiding (Complex)
import Data.Data

type Z = Integer
type R = Double
type C = C.Complex Double

deriving instance Ord (C.Complex Double)

data Number = Real R
            | Int Z
            | Complex C
            deriving (Eq, Ord, Data)

instance Show Number where
  show (Real r) = show r
  show (Int z) = show z
  show (Complex c) = show c

instance Z <: R where
  incl = fromInteger

instance Z <: C where
  incl = fromInteger

instance R <: C where
  incl = (0 :+)

compat :: (Number,Number) -> (Number,Number)
compat p@(Int n,Int m)          = p
compat   (Int n,r@(Real _))     = (Real (incl n),r)
compat   (Int n,c@(Complex _))  = (Complex (incl n),c)
compat   (Real n,Int m)         = (Real n, Real (incl m))
compat p@(Real _,Real _)        = p
compat   (Real n,c@(Complex _)) = (Complex (incl n),c)
compat p@(Complex _,Complex _)  = p
compat   (Complex n, Int m)     = (Complex n, Complex (incl m))
compat   (Complex n, Real m)    = (Complex n, Complex (incl m))


caseFun :: (Z -> Z) -> (R -> R) -> (C -> C) -> Number -> Number
caseFun zf rf cf (Real n)    = Real $ rf n
caseFun zf rf cf (Int n)     = Int $ zf n
caseFun zf rf cf (Complex n) = Complex $ cf n

caseFun2 :: (Z -> Z -> Z) -> (R -> R -> R) -> (C -> C -> C) -> Number -> Number -> Number
caseFun2 zf rf cf (Real n)    (Real m)    = Real $ rf n m
caseFun2 zf rf cf (Real n)    (Int m)     = Real $ rf n (incl m)
caseFun2 zf rf cf (Real n)    (Complex m) = Complex $ cf (incl n) m
caseFun2 zf rf cf (Int n)     (Real m)    = Real $ rf (incl n) m
caseFun2 zf rf cf (Int n)     (Int m)     = Int $ zf n m
caseFun2 zf rf cf (Int n)     (Complex m) = Complex $ cf (incl n) m
caseFun2 zf rf cf (Complex n) (Real m)    = Complex $ cf n (incl m)
caseFun2 zf rf cf (Complex n) (Int m)     = Complex $ cf n (incl m)
caseFun2 zf rf cf (Complex n) (Complex m) = Complex $ cf n m

instance Num Number where
  fromInteger = Int
  (+) = caseFun2 (+) (+) (+)
  (-) = caseFun2 (-) (-) (-)
  (*) = caseFun2 (*) (*) (*)
  abs = caseFun abs abs abs
  signum = caseFun signum signum signum

instance Fractional Number where
  (/) (Int x) (Int y) = Real $ fromInteger x / fromInteger y
  (/) x y = uncurry (/) . compat $ (x,y)
  recip (Int x) = Real . recip . fromInteger $ x
  recip x = caseFun undefined recip recip x
  fromRational = Real . fromRational

instance Floating Number where
  pi = Real pi
  exp (Int z) = Real $ exp $ fromInteger z
  exp (Real r) = Real $ exp r
  exp (Complex c) = Complex $ exp c
  log (Int z) = Real $ log $ fromInteger z
  log (Real r) = Real $ log r
  log (Complex c) = Complex $ log c
  sin (Int z) = Real $ sin $ fromInteger z
  sin (Real r) = Real $ sin r
  sin (Complex c) = Complex $ sin c
  cos (Int z) = Real $ cos $ fromInteger z
  cos (Real r) = Real $ cos r
  cos (Complex c) = Complex $ cos c
  asin (Int z) = Real $ asin $ fromInteger z
  asin (Real r) = Real $ asin r
  asin (Complex c) = Complex $ asin c
  acos (Int z) = Real $ acos $ fromInteger z
  acos (Real r) = Real $ acos r
  acos (Complex c) = Complex $ acos c
  atan (Int z) = Real $ atan $ fromInteger z
  atan (Real r) = Real $ atan r
  atan (Complex c) = Complex $ atan c
  sinh (Int z) = Real $ sinh $ fromInteger z
  sinh (Real r) = Real $ sinh r
  sinh (Complex c) = Complex $ sinh c
  cosh (Int z) = Real $ cosh $ fromInteger z
  cosh (Real r) = Real $ cosh r
  cosh (Complex c) = Complex $ cosh c
  asinh (Int z) = Real $ asinh $ fromInteger z
  asinh (Real r) = Real $ asinh r
  asinh (Complex c) = Complex $ asinh c
  acosh (Int z) = Real $ acosh $ fromInteger z
  acosh (Real r) = Real $ acosh r
  acosh (Complex c) = Complex $ acosh c
  atanh (Int z) = Real $ atanh $ fromInteger z
  atanh (Real r) = Real $ atanh r
  atanh (Complex c) = Complex $ atanh c
