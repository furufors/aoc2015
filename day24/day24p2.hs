#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (sortBy, subsequences)

type Weight = Int
type QE = Int

main :: IO ()
main = interact $ show . head . sortBy sortConfigurations . map minWeightQe . group . parsein

sortConfigurations :: (Weight, QE, [Weight]) -> (Weight, QE, [Weight]) -> Ordering
sortConfigurations (w1, qe1, _) (w2, qe2, _) = compare w1 w2 <> compare qe1 qe2

minWeightQe :: [Weight] -> (Weight, QE, [Weight])
minWeightQe a = (length a, product a, a)

group :: [Weight] -> [[Weight]]
group ws = let targetWeight = sum ws `div` 4
           in [ a | a <- subsequences ws, sum a == targetWeight]

parsein :: String -> [Weight]
parsein = map read . lines
