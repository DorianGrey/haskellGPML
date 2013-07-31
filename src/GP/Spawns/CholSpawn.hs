{-# LANGUAGE CPP             #-}

module GP.Spawns.CholSpawn (
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
                                                ksi = add k $ mapMatrix (* (sigma **2.0)) (ident (rows k)) -- K+sigma^2*I
                                                l = LA.chol ksi
                                                kInv = mXm (LA.inv $ trans l) (LA.inv l)
                                                alpha = mXv kInv y
                                                logL = foldVector (+) 0 $ mapVector log (takeDiag l)
                                                lPy = -0.5 * (y <.> alpha) - logL - (fromIntegral (rows x) :: Double)/2.0 * log (2.0 * pi :: Double) 
                                            in GP.CholSpawn kernel kernelDeriv theta sigma x l alpha lPy

