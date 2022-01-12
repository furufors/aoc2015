#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Control.Monad
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Either
import Data.List (isInfixOf)
import qualified Data.Map as M

type Bit16 = [Bool]
type Addr = String
type Registers = M.Map Addr Bit16
type AddrOrLit = Either Addr Int

main :: IO ()
main = do
    instructions <- getContents >>= return . parMap rpar (parseIn) . lines
    let endState = execState (sequence_ instructions) M.empty
    let res = M.lookup "a" endState
    case res of
         Nothing -> putStrLn "0"
         Just n  -> putStrLn . show . fromBit16 $ n

toBit16 :: Int -> Bit16
toBit16 = undefined

fromBit16 :: Bit16 -> Int
fromBit16 = undefined

parseIn :: String -> State Registers ()
parseIn s | isInfixOf "NOT" s    = let
                                   in update (fNot v1)
parseIn s | isInfixOf "AND" s    = let
                                   in update (fand v1 v2)
parseIn s | isInfixOf "OR" s     = let
                                   in update (for v1 v2)
parseIn s | isInfixOf "LSHIFT" s = let
                                   in update (for v1 v2)
parseIn s | isInfixOf "RSHIFT" s = let
                                   in update (for v1 v2)
parseIn s = undefined

update :: (Registers -> Registers) -> State Registers ()
update f = get >>= return f >>= put

substringSplit :: String -> String -> (String, String)
substringSplit = substringSplit' ""

substringSplit' :: String -> String -> String -> (String, String)
substringSplit' buf pf [] = error "No match"
substringSplit' buf pf s = if isPrefixOf pf s then (buf, drop (length pf) s) else substringSplit' (buf ++ [head s]) pf (tail s)

