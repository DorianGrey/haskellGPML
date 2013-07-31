{-# LANGUAGE CPP             #-}

module GP.Kernel.Kernel (
    KernelMxM,
    KernelVxM,
    KernelMxV,
    KernelVxV,
    KernelSet(..)    
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector

type KernelMxM = Double -> Matrix Double -> Matrix Double -> Matrix Double
type KernelVxM = Double -> Vector Double -> Matrix Double -> Vector Double
type KernelMxV = Double -> Matrix Double -> Vector Double -> Vector Double
type KernelVxV = Double -> Vector Double -> Vector Double -> Double

data KernelSet = KernelSet {
    mXm :: KernelMxM, 
    vXm :: KernelVxM,
    mXv :: KernelMxV,
    vXv :: KernelVxV   
}
