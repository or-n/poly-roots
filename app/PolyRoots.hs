{-# OPTIONS_GHC -O2 #-}

import Data.Complex
import Data.List (sortBy)
import Data.Function (on)
import System.Random

a @ b = sum (zipWith (*) a b)

exps value = iterate (* value) 1

getUpperLowerBounds p = (maxAbsHead + 1, recip (maxAbsLast + 1)) where
    maxAbsLast = maximum (map (magnitude . (/ last p)) (init p))
    maxAbsHead = maximum (map (magnitude . (/ head p)) (tail p))

initRoots p gen = take (length p - 1) (zipWith mkPolar radius angle) where
    (upper, lower) = getUpperLowerBounds p
    radius = randomRs (0, 1) gen
    angle = randomRs (0, 2 * pi) gen

findRoots p epsilon = initStdGen >>= go 0 . initRoots p where
    p' = zipWith (*) (map fromIntegral [1..]) (drop 1 p)
    go iter roots = do
        let offsets = map getOffset (zip roots [0..])
        let left = length (filter ((>= epsilon) . magnitude) offsets)
        putStrLn ("Iteration: " ++ show iter ++ ", Left: " ++ show left)
        if left == 0 then
            return roots
        else
            go (iter + 1) (zipWith (-) roots offsets)
        where
        getOffset (xi, i) = ratio / (1 - ratio * sigma) where
            evalRatio p p' expValues = p @ expValues / p' @ expValues
            ratio
                | magnitude xi <= 1 = evalRatio p p' (exps xi)
                | True = xi * evalRatio (reverse p) (reverse p') (exps (1 / xi))
            sigma = sum [1 / (xi - xj) | (xj, j) <- zip roots [0..], i /= j]

main = do
    content <- readFile "poly_coeff(997).txt"
    let values = map read (lines content) :: [Double]
    let polynomial = map (:+ 0) (reverse values)
    roots <- findRoots polynomial 1e-8
    let sortedRoots = sortBy (compare `on` realPart) roots
    mapM_ print sortedRoots