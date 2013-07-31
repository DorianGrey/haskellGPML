{-# LANGUAGE CPP             #-}

module GP.Spawns.SimpleSpawn (
    create
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import           Numeric.Container
import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified GP.Kernel.Kernel as K
import qualified GP.Spawns.Spawns as GP
                                
create :: Matrix Double -> Vector Double -> K.KernelSet -> K.KernelSet -> Double -> Double -> GP.Spawn
create x y kernel kernelDeriv theta sigma = let
                                                k = K.mXm kernel theta x x
                                                k' = LA.inv k
                                                alpha = mXv k' y
                                                n = fromIntegral (dim y) :: Double
                                                ksi = add k $ mapMatrix (* (sigma **2.0)) (ident (rows k))       -- K+sigma^2*I
                                                p1 = -0.5 * (vXm y (LA.inv ksi) <.> y)                         -- -0.5*(y^T)*((K+sigma^2*I)^-1)*y
                                                p2 = -0.5 * log (LA.det ksi) - (n/2) * log (2.0 * pi :: Double)  -- -0.5*log(|K+sigma^2*I|)-n/2*log(2*PI)  
                                            in GP.SimpleSpawn kernel kernelDeriv theta sigma x alpha (p1 + p2)
                                            