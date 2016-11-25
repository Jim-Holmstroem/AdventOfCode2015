{-# LANGUAGE OverloadedStrings #-}

import Data.Hash.MD5


hasAtleastNLeadingZeros :: Int -> String -> Bool
hasAtleastNLeadingZeros 0 _ = True
hasAtleastNLeadingZeros n ('0':str) = hasAtleastNLeadingZeros (n-1) str
hasAtleastNLeadingZeros _ _ = False

leading5ZeroHashes :: String -> [Int]
leading5ZeroHashes key = filter (hasAtleastNLeadingZeros 5 . md5s . Str . (key ++) . show) [1..]

main :: IO ()
main = head . leading5ZeroHashes . filter (/='\n') <$> getContents >>= print
