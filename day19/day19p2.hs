#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (splitWhen)

main :: IO ()
main = interact $ show . formula . toElem . last . lines

toElem :: String -> [String]
toElem [] = []
toElem s = ([head s]++(takeWhile (`elem` ['a'..'z']) (tail s))):(toElem (dropWhile (`elem` ['a'..'z']) (tail s)))

{- The number of iterations can be expressed as the below formula Rn, Ar and Y represents cases where the element length grows faster than unit length:
        Base case: Al => ThF       grows with 1 per iteration
   Base RnAr case: Al => ThRnFAr   grows with 3 per iteration (1 + Rn + Ar)
      Y+RnAr case: Ca => SiRnFYFAr grows with 5 per iteration (1 + Rn + Ar + Y + 1) (also multiple Ys, each giving 2 extra elements)
-}
formula :: [String] -> Int
formula s = let al = length s
                rn = length . filter (=="Rn") $ s
                ar = length . filter (=="Ar") $ s
                y  = length . filter (=="Y") $ s
            in al - rn - ar - 2*y - 1
