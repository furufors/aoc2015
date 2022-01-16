#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (subsequences)

main :: IO ()
main = interact $ show . length . filter ((==150) . sum) . subsequences . map read . lines
