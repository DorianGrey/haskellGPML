{-# LANGUAGE CPP             #-}

module GP.Spawns.CholSpawn (
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
                                 l :: Matrix Double,
                                 alpha :: Vector Double,
                                 lPy :: Double
                                }

create :: Matrix Double -> Vector Double -> K.KernelSet -> K.KernelSet -> Double -> Double -> SimpleSpawn
create x y kernel kernelDeriv theta sigma = let 
                                                k = (K.mXm kernel) theta x x
                                                ksi = add k $ mapMatrix (* (sigma **2.0)) (ident (rows k)) -- K+sigma^2*I
                                                l = LA.chol ksi
                                                kInv = mXm (LA.inv $ trans l) (LA.inv l)
                                                alpha = mXv kInv y
                                                logL = foldVector (+) 0 $ mapVector log (takeDiag l)
                                                lPy = -0.5 * (y <.> alpha) - logL - (fromIntegral (rows x) :: Double)/2.0 * log (2.0 * pi :: Double) 
                                            in SimpleSpawn kernel kernelDeriv theta sigma x l alpha lPy

calcPrediction :: Vector Double -> SimpleSpawn -> (Double, Double)
calcPrediction x' (SimpleSpawn kernel _ theta sigma x l alpha _) =  let
                                                                        k = (K.mXv kernel) theta x x'
                                                                        y' = k <.> alpha
                                                                        v = mXv (LA.inv (trans l)) k 
                                                                        var = ((K.vXv kernel) theta x' x') - (v <.> v) 
                                                                    in (y', sqrt var)