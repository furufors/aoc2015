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
        resolve addr cs = let expr = getExpr addr cs in show . fromBit16 $ resolveExpr cs expr

        resolveExpr :: Connections -> Expr -> Bit16
        resolveExpr cs (Not addr)      = let expr = getExpr addr cs
                                         in fNot (resolveExpr cs expr)
        resolveExpr cs (End addr)      = let axpr = getExpr addr cs
                                         in fAnd (toBit16 1) (resolveExpr cs axpr)
        resolveExpr cs (And addr bddr) = let rax = resolveExpr cs $ getExpr addr cs
                                             rbx = resolveExpr cs $ getExpr bddr cs
                                         in rax `par` (rbx `par` fAnd (rax) (rbx))
        resolveExpr cs (Or  addr bddr) = let rax = resolveExpr cs $ getExpr addr cs
                                             rbx = resolveExpr cs $ getExpr bddr cs
                                         in rax `par` (rbx `par` fOr  (rax) (rbx))
        resolveExpr cs (Rsh addr int ) = let expr = getExpr addr cs
                                         in fRsh (resolveExpr cs expr) int
        resolveExpr cs (Lsh addr int ) = let expr = getExpr addr cs
                                         in fLsh (resolveExpr cs expr) int
        resolveExpr cs (Val addr)      = let expr = getExpr addr cs
                                         in resolveExpr cs expr
        resolveExpr cs (Vil int )      = toBit16 int

        getExpr :: Addr -> Connections -> Expr
        getExpr addr cs = case M.lookup addr cs of
            Nothing -> error ("Missing expression for address: " ++ addr)
            Just e  -> e


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
