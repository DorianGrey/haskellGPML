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
import qualified GP.Spawns.Spawn as GPs
import qualified GP.Spawns.CholSpawn as CGP

theta_default = 0.3
sigma_default = 0.1

main::IO ()
main = do
    let x = fromLists [[235.0], [240.0], [245.0]]
    let xs = fromList [240.0]
    let y = fromList [36.0, 33.0, 30.0]
    -- General GP
    let gp = GPs.create x y SE.getDefaultSet SE.getDerivSet theta_default sigma_default
    let res = GPs.calcPrediction xs gp
    putStrLn "Results from Basic GP:"
    print $ "[alpha] " ++ show (GPs.alpha gp)
    print $ "[lPy] " ++ show (GPs.lPy gp)
    print $ "[pred. result] " ++ show res
    -- Cholesky Gp
    let gp = CGP.create x y SE.getDefaultSet SE.getDerivSet theta_default sigma_default
    let res = CGP.calcPrediction xs gp
    putStrLn "Results from Basic GP:"
    print $ "[alpha] " ++ show (CGP.alpha gp)
    print $ "[lPy] " ++ show (CGP.lPy gp)
    print $ "[pred. result] " ++ show res

    
