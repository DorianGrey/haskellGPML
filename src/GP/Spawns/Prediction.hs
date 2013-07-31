{-# LANGUAGE CPP             #-}

module GP.Spawns.Prediction (
    calcPrediction                                                                       
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import           Numeric.Container
import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified GP.Kernel.Kernel as K
import           GP.Spawns.Spawns (Spawn( SimpleSpawn ), Spawn( CholSpawn ))

calcPrediction :: Vector Double -> Spawn -> (Double, Double)
calcPrediction x' (SimpleSpawn kernel _ theta sigma x alpha _) =let 
                                                                    k1 = K.mXm kernel theta x x
                                                                    k = K.mXv kernel theta x x'
                                                                    y' = k <.> alpha 
                                                                    pt1 = K.vXv kernel theta x' x'
                                                                    pt2 = K.mXv kernel theta x x'
                                                                    pt25 = mapMatrix (* (sigma **2.0)) (ident (rows x))
                                                                    pt3 = LA.inv (add k1 pt25)
                                                                    pt4 = K.vXm kernel theta x' x
                                                                    var = pt1 - (pt2 <.> mXv pt3 pt4)
                                                                in (y', sqrt var)
calcPrediction x' (CholSpawn kernel _ theta _ x l alpha _) =    let
                                                                    k = K.mXv kernel theta x x'
                                                                    y' = k <.> alpha
                                                                    v = mXv (LA.inv (trans l)) k 
                                                                    var = K.vXv kernel theta x' x' - (v <.> v) 
                                                                in (y', sqrt var)