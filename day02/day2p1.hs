#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.Parallel.Strategies
import Data.List.Split (splitWhen)

main :: IO ()
main = interact $ show . sum . parMap rpar (toArea . parseIn) . lines
    where
        toArea :: (Int, Int, Int) -> Int
        toArea (l, w, h) = let a = l*w
                               b = w*h
                               c = h*l
                           in 2*a + 2*b + 2*c + minimum [a,b,c]

        parseIn :: String -> (Int, Int, Int)
        parseIn s = let (a:b:c:_) = splitWhen (=='x') s in (read a, read b, read c)
