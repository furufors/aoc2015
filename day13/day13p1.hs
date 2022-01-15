#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Algorithm.Search (dijkstra)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (nub, isPrefixOf, permutations)
import Debug.Trace (trace)

type Guest = String
type DeltaMood = Int
type Seated = [Guest]
type Guests = S.Set Guest
type MoodChange = M.Map (Guest, Guest) DeltaMood

main :: IO ()
main = interact $ show . solve' . M.fromList  . map parsein . lines

parsein :: String -> ((Guest, Guest), DeltaMood)
parsein s = let (a,rst) = substringSplit " would " s
                (c,b) = substringSplit " happiness units by sitting next to " rst
                d = init b
                hap = if isPrefixOf "lose " c
                      then read $ drop 5 c
                      else (-1) * (read $ drop 5 c)
            in ((a,d), hap)
    where
        substringSplit :: String -> String -> (String, String)
        substringSplit = substringSplit' ""

        substringSplit' :: String -> String -> String -> (String, String)
        substringSplit' buf pf [] = error "No match"
        substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)

solve' :: MoodChange -> Int
solve' changes =
    let neighborList = M.keys changes
        guests = nub $ (map fst neighborList) ++ (map snd neighborList)
        perms = permutations guests
        seatings = map (\l -> (last l):l) perms
        costs = map (\seating -> (sum . map cost) $ zip seating (tail seating)) seatings
    in (-1) * minimum costs
    where
        cost :: (Guest, Guest) -> Int
        cost (a,b) = let x = case M.lookup (a,b) changes of
                                Nothing -> error ("Missing mood-change " ++ a ++ " -> " ++ b)
                                Just d  -> d
                         y = case M.lookup (b,a) changes of
                                Nothing -> error ("Missing mood-change " ++ a ++ " -> " ++ b)
                                Just d  -> d
                     in x + y

solve :: MoodChange -> Int -- Todo: Ensure Dijkstra variant of solution is correct.
solve changes =
    let neighborList = M.keys changes
        guests = S.fromList $ (map fst neighborList) ++ (map snd neighborList)
        res = dijkstra (neighbors guests neighborList) (transitionCost changes) (solutionFound guests) []
    in case res of
        Nothing -> error "No solution found"
        Just n  -> trace (show (snd n)) ((-1) * (fst n))
    where
        neighbors :: Guests -> [(Guest, Guest)] -> Seated -> [Seated]
        neighbors gs ns [     ] = [x:y:[] | let guests = S.toList gs, x <- guests, y <- guests, x /= y]
        neighbors gs ns seated | S.fromList seated == gs = [(last seated):seated] -- Final closing step
        neighbors gs ns seated = let sNext = S.fromList . map snd $ filter ((== head seated) . fst) ns
                                     sSeated = S.fromList seated
                                     unseatedNeighbors = S.toList $ sNext S.\\ sSeated
                                 in [next:seated | next <- unseatedNeighbors]


        transitionCost :: MoodChange -> Seated -> Seated -> DeltaMood
        --transitionCost changes [] _______ = 0 -- First move is free
        transitionCost changes __ [     ] = error "Cannot go to empty state"
        transitionCost changes __ [  a  ] = error "Cannot go to singleton state"
        transitionCost changes __ (a:b:_) = let x = case M.lookup (a,b) changes of
                                                        Nothing -> error ("Missing mood-change " ++ a ++ " -> " ++ b)
                                                        Just d  -> d
                                                y = case M.lookup (b,a) changes of
                                                        Nothing -> error ("Missing mood-change " ++ a ++ " -> " ++ b)
                                                        Just d  -> d
                                            in x + y

        solutionFound :: Guests -> Seated -> Bool
        solutionFound guests [    ] = False
        solutionFound guests seated = (S.size guests + 1) == length seated && head seated == last seated
