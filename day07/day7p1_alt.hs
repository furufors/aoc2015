#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.Parallel
import Control.Parallel.Strategies
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Text.Read (readMaybe)

type Bit16 = [Bool]
type Addr = String
data Expr = Not Addr | And Addr Addr | End Addr | Or Addr Addr | Rsh Addr Int | Lsh Addr Int | Vil Int | Val Addr
type Connections = M.Map Addr Expr

main :: IO ()
main = interact $ resolve "a" . M.fromList . parMap rpar parseIn . lines
    where
        resolve :: String -> Connections -> String
        resolve addr cs = let expr = getVil addr (deepresolve cs) in show expr

        deepresolve :: Connections -> Connections
        deepresolve cs | all isVil (M.elems cs) = cs
        deepresolve cs = deepresolve $ foldl updateConnections cs (M.toList cs)

        isVil :: Expr -> Bool
        isVil (Vil _) = True
        isVil _       = False

        updateConnections :: Connections -> (Addr, Expr) -> Connections
        updateConnections cs (out, (Not addr))      = case getVil addr cs of
                                                        Just i    -> let i' = fromBit16 . fNot $ toBit16 i
                                                                     in M.insert out (Vil i') cs
                                                        otherwise -> cs
        updateConnections cs (out, (End addr))      = case getVil addr cs of
                                                        Just i    -> let i' = fromBit16 $ fAnd (toBit16 1) (toBit16 i)
                                                                     in M.insert out (Vil i') cs
                                                        otherwise -> cs
        updateConnections cs (out, (And addr bddr)) = case (getVil addr cs, getVil bddr cs)  of
                                                        (Just i, Just j) -> let i' = fromBit16 $ fAnd (toBit16 i) (toBit16 j)
                                                                     in M.insert out (Vil i') cs
                                                        otherwise -> cs
        updateConnections cs (out, (Or  addr bddr)) = case (getVil addr cs, getVil bddr cs)  of
                                                        (Just i, Just j) -> let i' = fromBit16 $ fOr (toBit16 i) (toBit16 j)
                                                                     in M.insert out (Vil i') cs
                                                        otherwise -> cs
        updateConnections cs (out, (Rsh addr int )) = case getVil addr cs of
                                                        Just i    -> let i' = fromBit16 $ fRsh (toBit16 i) int
                                                                     in M.insert out (Vil i') cs
                                                        otherwise -> cs
        updateConnections cs (out, (Lsh addr int )) = case getVil addr cs of
                                                        Just i    -> let i' = fromBit16 $ fLsh (toBit16 i) int
                                                                     in M.insert out (Vil i') cs
                                                        otherwise -> cs
        updateConnections cs (out, (Val addr))      = case getVil addr cs of
                                                        Just i    -> M.insert out (Vil i) cs
                                                        otherwise -> cs
        updateConnections cs (out, (Vil int ))      = cs

        getExpr :: Addr -> Connections -> Expr
        getExpr addr cs = case M.lookup addr cs of
            Nothing -> error ("Missing expression for address: " ++ addr)
            Just e  -> e

        getVil :: Addr -> Connections -> Maybe Int
        getVil addr cs = case getExpr addr cs of
            (Vil int) -> Just int
            otherwise -> Nothing

        parseIn :: String -> (Addr, Expr)
        parseIn s | isInfixOf "NOT"    s = let (addr,  out) = substringSplit " -> " (drop 4 s) in (out, Not addr)
        parseIn s | isPrefixOf "1 AND" s = let (addr,  out) = substringSplit " -> " (drop 6 s) in (out, End addr)
        parseIn s | isInfixOf "AND"    s = let (addr, rest) = substringSplit " AND " s
                                               (bddr,  out) = substringSplit " -> " rest in (out, And addr bddr)
        parseIn s | isInfixOf "OR"     s = let (addr, rest) = substringSplit " OR " s
                                               (bddr,  out) = substringSplit " -> " rest in (out, Or  addr bddr)
        parseIn s | isInfixOf "RSHIFT" s = let (addr, rest) = substringSplit " RSHIFT " s
                                               (offs,  out) = substringSplit " -> " rest in (out, Rsh  addr (rd offs))
        parseIn s | isInfixOf "LSHIFT" s = let (addr, rest) = substringSplit " LSHIFT " s
                                               (offs,  out) = substringSplit " -> " rest in (out, Lsh  addr (rd offs))
        parseIn s = let (left, out) = substringSplit " -> " s
                    in if (head left) `elem` ['0'..'9']
                       then (out, Vil (rd left))
                       else (out, Val left)

        rd :: Read a => String -> a
        rd a = case readMaybe a of
            Nothing -> error ("Cannot parse: " ++ a)
            Just b  -> b

        toBit16 :: Int -> Bit16
        toBit16 i = fst $ foldl step ([],i) [15,14..0]
            where
                step :: ([Bool], Int) -> Int -> ([Bool], Int)
                step (bs, n) i = if n >= 2^i then (bs ++ [True], n - 2^i) else (bs ++ [False], n)

        fromBit16 :: Bit16 -> Int
        fromBit16 = sum . zipWith (\a b -> if b then 2^a else 0) [15,14..0]

        substringSplit :: String -> String -> (String, String)
        substringSplit = substringSplit' ""

        substringSplit' :: String -> String -> String -> (String, String)
        substringSplit' buf pf [] = error "No match"
        substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)

        fNot :: Bit16 -> Bit16
        fNot = map not

        fAnd ::Bit16 -> Bit16 -> Bit16
        fAnd = zipWith (&&)

        fOr :: Bit16 -> Bit16 -> Bit16
        fOr = zipWith (||)

        fLsh :: Bit16 -> Int -> Bit16
        fLsh b i = take 16 (drop i b ++ repeat False)

        fRsh :: Bit16 -> Int -> Bit16
        fRsh b i = take 16 (take i (repeat False) ++ b)
