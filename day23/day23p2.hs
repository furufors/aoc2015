#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
data Reg = A | B
data Inst = Hlf Reg
          | Tpl Reg
          | Inc Reg
          | Jmp Int
          | Jie Reg Int
          | Jio Reg Int
type Prog = [Inst]

main :: IO ()
main = interact $ show . evaluate 0 (1,0) . map parseInst . lines

evaluate :: Int -> (Int, Int) -> Prog -> (Int, Int)
evaluate ip r     p | ip > (length p - 1) = r
evaluate ip (a,b) p = case p!!ip of
    Hlf A -> evaluate (ip + 1) (a `div` 2, b) p
    Hlf B -> evaluate (ip + 1) (a, b `div` 2) p
    Tpl A -> evaluate (ip + 1) (a * 3, b) p
    Tpl B -> evaluate (ip + 1) (a, b * 3) p
    Inc A -> evaluate (ip + 1) (a + 1, b) p
    Inc B -> evaluate (ip + 1) (a, b + 1) p
    Jmp i -> evaluate (ip + i) (a,b) p
    Jie A i -> if even a
               then evaluate (ip + i) (a,b) p
               else evaluate (ip + 1) (a,b) p
    Jie B i -> if even b
               then evaluate (ip + i) (a,b) p
               else evaluate (ip + 1) (a,b) p
    Jio A i -> if a == 1
               then evaluate (ip + i) (a,b) p
               else evaluate (ip + 1) (a,b) p
    Jio B i -> if b == 1
               then evaluate (ip + i) (a,b) p
               else evaluate (ip + 1) (a,b) p

parseInst :: String -> Inst
parseInst str =
    let r = reg (drop 4 str)
    in case take 3 str of
        "hlf" -> Hlf r
        "tpl" -> Tpl r
        "inc" -> Inc r
        "jmp" -> Jmp (int (drop 4 str))
        "jie" -> Jie r (int (drop 7 str))
        "jio" -> Jio r (int (drop 7 str))
        otherwise -> error $ "missing instruction" ++ str

int :: String -> Int
int ('+':s) = read s
int ('-':s) = (-1) * (read s)
int s       = error $ "cannot parse " ++ s

reg :: String -> Reg
reg ('a':_) = A
reg ('b':_) = B
reg s   = error $ "unknown register " ++ s
