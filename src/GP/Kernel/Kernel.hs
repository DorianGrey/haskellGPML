{-# LANGUAGE CPP             #-}

module GP.Kernel.Kernel (
    Kernel_MxM,
    Kernel_VxM,
    Kernel_MxV,
    Kernel_VxV,
    KernelSet(..)    
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector

type Kernel_MxM = Double -> Matrix Double -> Matrix Double -> Matrix Double
type Kernel_VxM = Double -> Vector Double -> Matrix Double -> Vector Double
type Kernel_MxV = Double -> Matrix Double -> Vector Double -> Vector Double
type Kernel_VxV = Double -> Vector Double -> Vector Double -> Double

data KernelSet = KernelSet {
    mXm :: Kernel_MxM, 
    vXm :: Kernel_VxM,
    mXv :: Kernel_MxV,
    vXv :: Kernel_VxV   
}
