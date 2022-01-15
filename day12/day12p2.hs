#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (isInfixOf)

data Object = Object String [Object]

main :: IO ()
main = interact $ show . sum . toListOfInt . removeRedObjects . toObject . head . lines
    where
        toListOfInt :: String -> [Int]
        toListOfInt [] = []
        toListOfInt str = let (a,b) = span (\c -> elem c ('-':['0'..'9'])) str
                          in if a == [] then toListOfInt (tail str) else (read a):(toListOfInt b)

        removeRedObjects :: Object -> String
        removeRedObjects (Object content objs) = if isInfixOf ":\"red\"" content then "" else content ++ concat (map removeRedObjects objs)

        toObject :: String -> Object
        toObject str = let depth = atDetph 0 str
                           pairs = zip str depth
                           contents = map fst $ filter ((==1) . snd) pairs
                           objects  = map toObject $ group pairs
                       in Object contents objects

        atDetph :: Int -> String -> [Int]
        atDetph _ [] = []
        atDetph i ('{':ss) = (i+1):(atDetph (i+1) ss)
        atDetph i ('}':ss) = i:(atDetph (i-1) ss)
        atDetph i ( s:ss ) = i:(atDetph i ss)

        group :: [(Char,Int)] -> [String]
        group [] = []
        group ((s,1):ss) = group ss
        group ((s,i):ss) = let (cont, rest) = span ((>1) . snd) ss in (s:map fst cont):(group rest)
