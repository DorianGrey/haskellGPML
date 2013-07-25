{-# LANGUAGE CPP             #-}
-- {-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  betterFind
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
import           Numeric.Container
import qualified GP.Kernel.SquaredExponential as SE
import qualified GP.Spawns.Spawn as GPs

theta_default = 0.3
sigma_default = 0.1

main::IO ()
main = do
    let x = fromLists [[235.0], [240.0], [245.0]]
    let xs = fromList [240.0]
    let y = fromList [36.0, 33.0, 30.0]
    let gp = GPs.create x y SE.getDefaultSet SE.getDerivSet theta_default sigma_default
    let res = GPs.calcPrediction xs gp
    print $ GPs.alpha gp
    print $ GPs.lPy gp
    print res

    
