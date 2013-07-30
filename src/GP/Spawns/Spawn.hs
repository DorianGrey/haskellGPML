{-# LANGUAGE CPP             #-}

module GP.Spawns.Spawn (
    create,
    SimpleSpawn(..),
    calcPrediction
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import           Numeric.Container
import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified GP.Kernel.Kernel as K

data SimpleSpawn = SimpleSpawn { kernel :: K.KernelSet,
                                 kernelDeriv :: K.KernelSet,
                                 theta :: Double,
                                 sigma :: Double,
                                 x :: Matrix Double,
                                 alpha :: Vector Double,
                                 lPy :: Double
                                }
                                
create :: Matrix Double -> Vector Double -> K.KernelSet -> K.KernelSet -> Double -> Double -> SimpleSpawn
create x y kernel kernelDeriv theta sigma = let
                                                k = (K.mXm kernel) theta x x
                                                k' = LA.inv k
                                                alpha = mXv k' y
                                                alpha' = alpha <.> alpha
                                                negInv = mapMatrix (+alpha') (mapMatrix (*(-1)) k')              -- k' *(-1) + alpha' -> does not seems there is an easier way 
                                                kdiv = (K.mXm kernelDeriv) theta x x
                                                n = fromIntegral (dim y) :: Double
                                                ksi = add k $ mapMatrix (* (sigma **2.0)) (ident (rows k))       -- K+sigma^2*I
                                                p1 = -0.5 * ((vXm y (LA.inv ksi)) <.> y)                         -- -0.5*(y^T)*((K+sigma^2*I)^-1)*y
                                                p2 = -0.5 * log (LA.det ksi) - (n/2) * log (2.0 * pi :: Double)  -- -0.5*log(|K+sigma^2*I|)-n/2*log(2*PI)  
                                            in SimpleSpawn kernel kernelDeriv theta sigma x alpha (p1 + p2)

calcPrediction :: Vector Double -> SimpleSpawn -> (Double, Double)
calcPrediction x' (SimpleSpawn kernel _ theta sigma x alpha _) =    let 
                                                                        k1 = (K.mXm kernel) theta x x
                                                                        k = (K.mXv kernel) theta x x'
                                                                        y' = k <.> alpha 
                                                                        pt1 = (K.vXv kernel) theta x' x'
                                                                        pt2 = (K.mXv kernel) theta x x'
                                                                        pt25 = (mapMatrix (* (sigma **2.0)) (ident (rows x)))
                                                                        pt3 = LA.inv (add k1 pt25)
                                                                        pt4 = (K.vXm kernel) theta x' x
                                                                        var = pt1 - (pt2 <.> (mXv pt3 pt4))
                                                                    in (y', sqrt var)

                                            