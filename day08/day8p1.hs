#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.Parallel.Strategies

main :: IO ()
main = interact $ show . sum . parMap rpar (\l -> length l - (substlength l 0)) . lines

substlength :: String -> Int -> Int
substlength []                i = i
substlength ('\\':'\\':ss)    i = substlength ss (i + 1)
substlength ('\\':'x':_:_:ss) i = substlength ss (i + 1)
substlength ('\\':_:ss)       i = substlength ss (i + 1)
substlength ('"':ss)          i = substlength ss (i)
substlength (_:ss)            i = substlength ss (i + 1)
