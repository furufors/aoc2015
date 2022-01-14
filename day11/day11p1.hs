#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.Parallel.Strategies
import Data.Char
import Data.List (isInfixOf)
import Debug.Trace (trace)

main :: IO ()
main = interact $ show . parMap rpar nextValid . lines
    where
        nextValid :: String -> String
        nextValid str = let next = increment str
                        in if valid next then next else nextValid next

        valid :: String -> Bool
        valid s = req1 s && req2 s && req3 s

        req1 :: String -> Bool
        req1 s = any (\t -> isInfixOf t s) sequences

        req2 :: String -> Bool
        req2 s = not (elem 'i' s || elem 'o' s || elem 'l' s)

        req3 :: String -> Bool
        req3 s = length (filter (id) $ map (\t -> isInfixOf t s) doubles) >= 2

        increment :: String -> String
        increment [] = []
        increment s = if last s == 'z' then increment (init s) ++ ['a'] else (init s) ++ [chr (ord (last s) + 1)]

doubles :: [String]
doubles = [ [l, l] | l <- ['a'..'z']]

sequences :: [String]
sequences = [ [chr x, chr (x+1), chr (x+2)] | l <-['a'..'x'], let x = ord l ]
