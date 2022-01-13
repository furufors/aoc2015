#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Algorithm.Search (dijkstra)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (nub, isPrefixOf)
import Debug.Trace (trace)

type City = String
type Distance = Int
type Visited = [City]
type Cities = S.Set City
type Distances = M.Map (City, City) Distance

main :: IO ()
main = interact $ show . solve . M.fromList . concat . map parsein . lines

parsein :: String -> [((City, City), Distance)]
parsein s = let (cs,cst) = substringSplit " = " s
                (a,b) = substringSplit " to " cs
                dist = read cst
            in [((a,b), dist), ((b,a), dist)]
    where
        substringSplit :: String -> String -> (String, String)
        substringSplit = substringSplit' ""

        substringSplit' :: String -> String -> String -> (String, String)
        substringSplit' buf pf [] = error "No match"
        substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)

solve :: Distances -> Int
solve dists =
    let neighborList = M.keys dists
        cities = S.fromList $ (map fst neighborList) ++ (map snd neighborList)
        res = dijkstra (neighbors neighborList) (transitionCost dists) (solutionFound cities) []
    in case res of
        Nothing -> error "No solution found"
        Just n  -> trace (show (snd n)) (fst n)
    where
        neighbors :: [(City, City)] -> Visited -> [Visited]
        neighbors ns [     ] = map (:[]) . nub $ (map fst ns) ++ (map snd ns)
        neighbors ns visited = let sNext = S.fromList . map snd $ filter ((== head visited) . fst) ns
                                   sVisited = S.fromList visited
                                   unvisitedNeighbors = S.toList $ sNext S.\\ sVisited
                               in [next:visited | next <- unvisitedNeighbors]


        transitionCost :: Distances -> Visited -> Visited -> Distance
        transitionCost distances [    ] ______ = 0 -- First move is free
        transitionCost distances ______ [    ] = error "Cannot go to empty state"
        transitionCost distances (a:as) (b:bs) = case M.lookup (a,b) distances of
            Nothing -> error ("Missing distance " ++ a ++ " -> " ++ b)
            Just d  -> d

        solutionFound :: Cities -> Visited -> Bool
        solutionFound cities visited = cities == (S.fromList visited)
