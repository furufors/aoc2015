#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE LambdaCase #-}
main :: IO ()
main = interact $ show . sum . map (\case '(' -> 1; ')' -> -1; _ -> 0)