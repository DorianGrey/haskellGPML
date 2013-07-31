{-# LANGUAGE CPP             #-}

module GP.Kernel.SquaredExponential (
    calculateMxM,
    calculateDerivMxM,
    calculateVxV,
    calculateDerivVxV,
    getDefaultSet,
    getDerivSet
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import           Numeric.Container
import           GP.Kernel.Kernel

calculateDerivVxV :: KernelVxV
calculateDerivVxV l x x' =  let diff = sub x x'
                                b    = abs $ dot diff diff 
                            in exp (-0.5 / l * b) * (-0.5 * b / l**2)

calculateDerivVxM :: KernelVxM
calculateDerivVxM l x x' = fromList $ map (calculateDerivVxV l x) (toRows x')

calculateDerivMxV :: KernelMxV
calculateDerivMxV l x x' = fromList $ map (calculateDerivVxV l x') (toRows x) -- Just swapping parameters (corresponding to function above) is ok here, since abs(diff(x, x')) is used

calculateVxV :: KernelVxV
calculateVxV l x x' = let diff = mapVector abs $ sub x x'
                in exp (-0.5 * abs (dot diff diff / l))

calculateVxM :: KernelVxM
calculateVxM l x x' = fromList $ map (calculateVxV l x) (toRows x')

calculateMxV :: KernelMxV
calculateMxV l x x' = fromList $ map (calculateVxV l x') (toRows x) -- Just swapping parameters (corresponding to function above) is ok here, since abs(diff(x, x')) is used

{- 
    Combine each row in x with each one in x'. The calculateVxV function is used for performing the combination.
    The result is stored in a result matrix.

    for row in range(len(X)):
        for col in range(len(Y)):
            result[row, col] = calculateVxV(X[row], Y[col], l)
    
    calculateVxV performs the row x row calculation.
    Due to the nature of the kernel function - being based on abs diff () - MxV and VxM will produce the same result, as long as both parameters are the same.

-}
calculateMxM :: KernelMxM
calculateMxM l x x' =  let xx' = toRows x'
                       in fromRows $ map (calculateMxV l x) xx' -- Note: Parameters are swapped. Is ok here, due to the kernels nature 
{-
    Unless proven otherwise, those are equivalent, except the given functor
-}
calculateDerivMxM :: KernelMxM
calculateDerivMxM l x x' = let xx' = toRows x'
                            in fromRows $ map (calculateDerivMxV l x) xx'

getDefaultSet :: KernelSet
getDefaultSet = KernelSet calculateMxM calculateVxM calculateMxV calculateVxV

getDerivSet :: KernelSet
getDerivSet = KernelSet calculateDerivMxM calculateDerivVxM calculateDerivMxV calculateDerivVxV
