#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char (ord)
import Data.Digest.Pure.MD5
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C8


main :: IO ()
main = putStrLn . show . fst . head . dropWhile (not . isPrefixOf ("000000") . snd) $ generator
    where
        generator :: [(Int, String)]
        generator = [(i, show . md5 . C8.pack $ "ckczppom" ++ show i) | i <- [1..]]
#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
