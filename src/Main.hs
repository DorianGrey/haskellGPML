{-# LANGUAGE CPP             #-}
-- {-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  ./.
--
-- Maintainer  :  DorianGrey
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
        main
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import qualified GP.Kernel.SquaredExponential as SE
import qualified GP.Spawns.SimpleSpawn as GPs
import qualified GP.Spawns.CholSpawn as CGP
import qualified GP.Spawns.Spawns as GP
import           GP.Spawns.Prediction

thetaDefault :: Double 
thetaDefault = 0.3

sigmaDefault :: Double
sigmaDefault = 0.1

main::IO ()
main = do
    let x = fromLists [[235.0], [240.0], [245.0]]
    let xs = fromList [240.0]
    let y = fromList [36.0, 33.0, 30.0]
    -- General GP
    let gp = GPs.create x y SE.getDefaultSet SE.getDerivSet thetaDefault sigmaDefault
    let res = calcPrediction xs gp
    putStrLn "Results from Basic GP:"
    print $ "[alpha] " ++ show (GP.alpha gp)
    print $ "[lPy] " ++ show (GP.lPy gp)
    print $ "[pred. result] " ++ show res
    -- Cholesky Gp
    let cgp = CGP.create x y SE.getDefaultSet SE.getDerivSet thetaDefault sigmaDefault
    let cres = calcPrediction xs cgp
    putStrLn "Results from Basic GP:"
    print $ "[alpha] " ++ show (GP.alpha cgp)
    print $ "[lPy] " ++ show (GP.lPy cgp)
    print $ "[pred. result] " ++ show cres

    
