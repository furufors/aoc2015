#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE LambdaCase #-}
main :: IO ()
main = interact $ show . (+1) . length . takeWhile (>= 0) . scanl1 (+) . map (\case '(' -> 1; ')' -> -1; _ -> 0)


-- main = interact $ show . countUntilNegative 0 0
--     where
--         countUntilNegative :: Int -> Int -> String -> Int
--         countUntilNegative n i [    ] = error "Reached end of string before the basement"
--         countUntilNegative n i (s:ss)
--             | n < 0     = i
--             | otherwise = case s of
--                         '(' -> countUntilNegative (n+1) (i+1) ss
--                         ')' -> countUntilNegative (n-1) (i+1) ss
--                         ___ -> error "Non parenthesis characther"
