#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE LambdaCase #-}

{- Config for testcase or input -}
steps :: Int
steps = 100
lights :: [Int]
lights = [0..99]

main :: IO ()
main = interact $ show . countOn . (\i -> (iterate step i)!!steps) . parsein . lines

parsein :: [String] -> [[Bool]]
parsein [    ] = []
parsein (s:ss) = (map (\case '#' -> True; '.' -> False) s):(parsein ss)

step :: [[Bool]] -> [[Bool]]
step a = [ [ next
           | x <- lights
           , let n1 = get a (x-1) (y-1)
           , let n2 = get a (x-1) (y  )
           , let n3 = get a (x-1) (y+1)
           , let n4 = get a (x  ) (y-1)
           , let n5 = get a (x  ) (y+1)
           , let n6 = get a (x+1) (y-1)
           , let n7 = get a (x+1) (y  )
           , let n8 = get a (x+1) (y+1)
           , let ns = length . filter id $ [n1,n2,n3,n4,n5,n6,n7,n8]
           , let res = case get a x y of
                            True  -> if ns == 2 || ns == 3 then True else False
                            False -> if ns == 3 then True else False
           , let next = if (x == head lights || x == last lights) && (y == head lights || y == last lights)
                        then True
                        else res
           ]
         | y <- lights
         ]

get :: [[Bool]] -> Int -> Int -> Bool
get a x y | x < (head lights) = False
get a x y | x > (last lights) = False
get a x y | y < (head lights) = False
get a x y | y > (last lights) = False
get a x y = a!!y!!x

countOn :: [[Bool]] -> Int
countOn  = sum . map (sum . map (\case True -> 1; False -> 0))
