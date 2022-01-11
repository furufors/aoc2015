#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Control.Parallel.Strategies
import Data.List.Split (splitWhen)

main :: IO ()
main = interact $ show . sum . parMap rpar (toRibbon . parseIn) . lines
    where
        toRibbon :: (Int, Int, Int) -> Int
        toRibbon (l, w, h) = let a = l*w
                                 b = w*h
                                 c = h*l
                                 perimeter a b = 2 * (a + b)
                                 bow = l * w * h
                             in minimum [perimeter l w, perimeter w h, perimeter h l] + bow

        parseIn :: String -> (Int, Int, Int)
        parseIn s = let (a:b:c:_) = splitWhen (=='x') s in (read a, read b, read c)
