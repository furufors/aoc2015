#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.List (nub, isPrefixOf, permutations, sort)

type Guest = String
type DeltaMood = Int
type MoodChange = M.Map (Guest, Guest) DeltaMood

main :: IO ()
main = interact $ show . solve . M.fromList  . map parsein . lines

parsein :: String -> ((Guest, Guest), DeltaMood)
parsein s =
    let (a,rst) = substringSplit " would " s
        (c,b) = substringSplit " happiness units by sitting next to " rst
        d = init b
        hap = if isPrefixOf "lose " c
              then (-1) * (read $ drop 5 c)
              else read $ drop 5 c
    in ((a,d), hap)
    where
        substringSplit :: String -> String -> (String, String)
        substringSplit = substringSplit' ""

        substringSplit' :: String -> String -> String -> (String, String)
        substringSplit' buf pf [] = error "No match"
        substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)

solve :: MoodChange -> Int
solve changes =
    let neighborList = M.keys changes
        guests = nub $ (map fst neighborList) ++ (map snd neighborList)
        perms = permutations guests
        seatings = map (\l -> (last l):l) perms
        costs = map (\seating -> (sum . tail . sort . map cost) $ zip seating (tail seating)) seatings -- Place yourself between the worst pair
    in maximum costs
    where
        cost :: (Guest, Guest) -> Int
        cost (a,b) = let x = case M.lookup (a,b) changes of
                                Nothing -> error ("Missing mood-change " ++ a ++ " -> " ++ b)
                                Just d  -> d
                         y = case M.lookup (b,a) changes of
                                Nothing -> error ("Missing mood-change " ++ a ++ " -> " ++ b)
                                Just d  -> d
                     in x + y
