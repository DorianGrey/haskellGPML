{-# LANGUAGE CPP             #-}

module GP.Kernel.SquaredExponential (
    calculate_MxM,
    calculateDeriv_MxM,
    calculate_VxV,
    calculateDeriv_VxV,
    getDefaultSet,
    getDerivSet
) where

import           Data.Packed.Matrix
import           Data.Packed.Vector
import           Numeric.Container
import           GP.Kernel.Kernel

calculateDeriv_VxV :: Kernel_VxV
calculateDeriv_VxV l x x' = let diff = sub x x'
                                b    = abs $ dot diff diff 
                            in exp (-0.5 / l * b) * (-0.5 * b / l**2)

calculateDeriv_VxM :: Kernel_VxM
calculateDeriv_VxM l x x' = fromList $ map (calculateDeriv_VxV l x) (toRows x')

calculateDeriv_MxV :: Kernel_MxV
calculateDeriv_MxV l x x' = fromList $ map (calculateDeriv_VxV l x') (toRows x) -- Just swapping parameters (corresponding to function above) is ok here, since abs(diff(x, x')) is used

calculate_VxV :: Kernel_VxV
calculate_VxV l x x' = let diff = mapVector abs $ sub x x'
                in exp (-0.5 * abs (dot diff diff / l))

calculate_VxM :: Kernel_VxM
calculate_VxM l x x' = fromList $ map (calculate_VxV l x) (toRows x')

calculate_MxV :: Kernel_MxV
calculate_MxV l x x' = fromList $ map (calculate_VxV l x') (toRows x) -- Just swapping parameters (corresponding to function above) is ok here, since abs(diff(x, x')) is used

{- Performs the inner loop:
    for col in range(len(Y)):
        result[row, col] = calculate_VxV(X[row], Y[col], l)

    Note: Order of parameters is slightly reversed (l x' x instead of l x x'), since it eases up using map in `calculate`.
    map (calculate_VxV l x) x' results in a list of Double, thus -> fromList.
    As mentioned in the loop description above, the result counts as a row. 
-}
calcHelp :: Kernel_VxV -> Double -> [Vector Double] -> Vector Double -> Vector Double
calcHelp func l x' x = fromList $ map (func l x) x' 

{- 
    Combine each row in x with each one in x'. The calculate_VxV function is used for performing the combination.
    The result is stored in a result matrix.

    for row in range(len(X)):
        for col in range(len(Y)):
            result[row, col] = calculate_VxV(X[row], Y[col], l)
    
    calcHelp performs the internals of mapping as representation of the inner loop as mentioned above.
    Due to the inpretation, the map-result creates a list of row-vectors, thus, we need fromRows here.

-}
calculate_MxM :: Kernel_MxM
calculate_MxM l x x' =  let xx' = toRows x'
                        in fromColumns $ map (calculate_MxV l x) xx' -- Note: Parameters are swapped. Is ok here, due to the kernels nature 
{-
    Unless proven otherwise, those are equivalent, except the given functor
-}
calculateDeriv_MxM :: Kernel_MxM
calculateDeriv_MxM l x x' = let xx' = toRows x'
                            in fromRows $ map (calculateDeriv_MxV l x) xx'

getDefaultSet :: KernelSet
getDefaultSet = KernelSet calculate_MxM calculate_VxM calculate_MxV calculate_VxV

getDerivSet :: KernelSet
getDerivSet = KernelSet calculateDeriv_MxM calculateDeriv_VxM calculateDeriv_MxV calculateDeriv_VxV