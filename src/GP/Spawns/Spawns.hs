{-# LANGUAGE CPP             #-}

module GP.Spawns.Spawns (
    Spawn(..)                                                                        
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import qualified GP.Kernel.Kernel as K

data Spawn =    SimpleSpawn { kernel :: K.KernelSet, 
                              kernelDeriv :: K.KernelSet,
                              theta :: Double,
                              sigma :: Double,
                              x :: Matrix Double,
                              alpha :: Vector Double,
                              lPy :: Double
                             } |
                CholSpawn   { kernel :: K.KernelSet,
                              kernelDeriv :: K.KernelSet,
                              theta :: Double,
                              sigma :: Double,
                              x :: Matrix Double,
                              l :: Matrix Double,
                              alpha :: Vector Double,
                              lPy :: Double
                             }

