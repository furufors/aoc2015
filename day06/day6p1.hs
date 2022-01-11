#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE LambdaCase, BangPatterns #-}
import Data.List (isPrefixOf)
import Control.Parallel.Strategies

data Light = On | Off
data Action = TurnOn | Toggle | TurnOff
type Range = (Int, Int)
type Instruction = (Action, (Range, Range))
type Grid = [[Light]]

main :: IO ()
main = interact $ show . countOn . applySteps . parMap rpar parseIn . lines
    where
        applySteps :: [Instruction] -> Grid
        applySteps is = [[ foldl (step x y) Off is  | x <- [0..999] ] | y <- [0..999] ]

        step :: Int -> Int -> Light -> Instruction -> Light
        step x y l (a, ((xl, xh), (yl, yh))) = if x >= xl && x <= xh && y >= yl && y <= yh
                                               then update a l
                                               else l

        update :: Action -> Light -> Light
        update TurnOn  = const On
        update Toggle  = \case
            On  -> Off
            Off -> On
        update TurnOff = const Off

        parseIn :: String -> Instruction
        parseIn s | isPrefixOf "turn on"  s = ( TurnOn, xryr (drop 8 s))
        parseIn s | isPrefixOf "toggle"   s = ( Toggle, xryr (drop 7 s))
        parseIn s | isPrefixOf "turn off" s = (TurnOff, xryr (drop 9 s))

        xryr :: String -> (Range, Range)
        xryr s = let lr = substringSplit " through " s
                     starts = substringSplit "," $ fst lr
                     ends   = substringSplit "," $ snd lr
                 in ((read $ fst starts, read $ fst ends), (read $ snd starts, read $ snd ends))

        substringSplit :: String -> String -> (String, String)
        substringSplit = substringSplit' ""

        substringSplit' :: String -> String -> String -> (String, String)
        substringSplit' buf pf [] = error "No match"
        substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)


        countOn :: Grid -> Int
        countOn = sum . map (sum . map (\case On -> 1; Off -> 0))
