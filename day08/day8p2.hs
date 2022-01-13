#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.Parallel.Strategies

main :: IO ()
main = interact $ show . sum . parMap rpar (\l -> (substlength l 0) - length l) . lines

substlength :: String -> Int -> Int
substlength []        i = i + 2
substlength ('\\':ss) i = substlength ss (i + 2)
substlength ('"':ss)  i = substlength ss (i + 2)
substlength (_:ss)    i = substlength ss (i + 1)
