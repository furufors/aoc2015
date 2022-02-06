#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (sortBy)
type Armor    = Integer
type Cost     = Integer
type Damage   = Integer
type Hitpoins = Integer
type Stats    = (Hitpoins, Damage, Armor)

-- Maximum money that can be spent and still lose...
main :: IO ()
main = putStrLn . show . head . dropWhile ((==True) . snd) $ fmap (fmap (\stats -> battle stats (109, 8, 2))) (reverse confsByCost)

confsByCost :: [(Cost, Stats)]
confsByCost = sortBy (\a b -> compare (fst a) (fst b)) configurations

configurations :: [(Cost, Stats)]
configurations = [(cost, (100, damage, armor))
                 | (cw, dw, aw) <- weapons -- mandatory
                 , (ca, da, aa)  <- armors -- optional
                 , lr@(cr, dr, ar)  <- rings
                 , rr@(cl, dl, al)  <- rings
                 , if lr == (0,0,0) then True else lr /= rr -- Allow two empty
                 , let cost   = cw + ca + cr + cl
                 , let damage = dw + da + dr + dl
                 , let armor  = aw + aa + ar + al
                 ]

weapons :: [(Cost, Damage, Armor)]
weapons = [ (  8, 4, 0)
          , ( 10, 5, 0)
          , ( 25, 6, 0)
          , ( 40, 7, 0)
          , ( 74, 8, 0)
          ]
armors :: [(Cost, Damage, Armor)]
armors  = [ ( 13, 0, 1)
          , ( 31, 0, 2)
          , ( 53, 0, 3)
          , ( 75, 0, 4)
          , (102, 0, 5)
          , (  0, 0, 0)
          ]
rings :: [(Cost, Damage, Armor)]
rings   = [ ( 25, 1, 0)
          , ( 50, 2, 0)
          , (100, 3, 0)
          , ( 20, 0, 1)
          , ( 40, 0, 2)
          , ( 80, 0, 3)
          , (  0, 0, 0)
          ]

-- Returns True if the first player wins
battle :: Stats -> Stats -> Bool
battle a b = let b1@(bh1,_,_) = turn a b
                 a1@(ah1,_,_) = turn b a
             in if bh1 <= 0 then True
                else if ah1 <= 0 then False
                     else battle a1 b1
    where
        turn :: Stats -> Stats -> Stats
        turn (ahp, adm, aar) (bhp, bdm, bar) =
            let damage = max (adm - bar) 1
            in (bhp - damage, bdm, bar)
