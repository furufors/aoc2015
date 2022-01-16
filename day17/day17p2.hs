#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (group, sort, subsequences)

main :: IO ()
main = interact $ show . length . head . group . sort . map length . filter ((==150) . sum) . subsequences . map read . lines
