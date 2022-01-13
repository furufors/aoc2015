#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . length . (\s -> (iterate lookAndSay s) !! 40) . head . lines

lookAndSay :: String -> String
lookAndSay [    ] = []
lookAndSay (a:as) = let (same,rest) = span (==a) as
                        nSame = length same
                    in show (nSame + 1) ++ [a] ++ lookAndSay rest
