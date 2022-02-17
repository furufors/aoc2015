#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (nub)
type Armor    = Integer
type Cost     = Integer
type Damage   = Integer
type Heal     = Integer
type Mana     = Integer
type Hitpoins = Integer
type Spent    = Integer
type Player   = (Hitpoins, Mana, Spent, Effects)
type Boss     = (Hitpoins, Damage)
data State    = Lost | Won Spent | Game Player Boss deriving (Eq)
type Effects  = (Integer, Integer, Integer) -- Shield Poison Recharge

main :: IO ()
main = putStrLn . show . leastMana 500000 . fightUntilResult $ [initState]

initState :: State
initState = Game (50, 500, 0, (0, 0, 0)) (51, 9)

leastMana :: Spent -> [State] -> Spent
leastMana n ([])   = n
leastMana n ((Lost    ):as) = leastMana n as
leastMana n ((Game _ _):as) = leastMana n as
leastMana n ((Won  m  ):as) = leastMana (min n m) as

fightUntilResult :: [State] -> [State]
fightUntilResult ss = if all ended ss
                      then ss
                      else fightUntilResult (nub . concat . map battle $ ss)
    where
        ended :: State -> Bool
        ended (Lost    ) = True
        ended (Won  _  ) = True
        ended (Game _ _) = False

convertEffects :: Effects -> (Armor, Damage, Mana)
convertEffects (s, p, r) = (if s > 0 then 7 else 0, if p > 0 then 3 else 0, if r > 0 then 101 else 0)

ageEffects :: Effects -> Effects
ageEffects (s, p, r) = (max 0 (s-1), max 0 (p-1), max 0 (r-1))

possibleActions :: Mana -> Effects -> [(Cost, Damage, Heal, Effects)]
possibleActions mana e@(s,p,r) =
    let shield   = if s == 0 then [(113, 0, 0, (6, p, r))] else []
        poison   = if p == 0 then [(173, 0, 0, (s, 6, r))] else []
        recharge = if r == 0 then [(229, 0, 0, (s, p, 5))] else []
        options  = [(53, 4, 0, e), (73, 2, 2, e)] ++ shield ++ poison ++ recharge
    in filter (\(c,_,_,_) -> c <= mana) options

battle :: State -> [State]
battle (Lost ) = []
battle (Won i) = [Won i]
battle (Game (php, pma, spnt, pef) (bhp, bdm))
    = [playerStep action | action <- possibleActions pma (ageEffects pef)]
    where
        playerStep (cst, cdm, chl, cef) =
            let (ear, edm, ema) = convertEffects pef
                bossNextHp = bhp - cdm - edm
                bossNext   = (bossNextHp, bdm)
            in if php - 1 <= 0 -- Hard mode
               then Lost
               else if bossNextHp <= 0
                    then Won (spnt + cst)
                    else let (ear2, edm2, ema2) = convertEffects cef
                             bossDamage = max (bdm - ear2) 1
                             playerNextHp = php - 1 + chl - bossDamage -- Hard mode
                             playerNext = (playerNextHp, pma - cst + ema + ema2, spnt + cst, ageEffects cef)
                             bossNextHp2 = bossNextHp - edm2
                             bossNext2 = (bossNextHp2, bdm)
                         in if bossNextHp2 <= 0
                            then Won (spnt + cst)
                            else if playerNextHp <= 0
                                 then Lost
                                 else Game playerNext bossNext2

