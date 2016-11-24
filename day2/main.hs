{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Applicative
import Data.Text (Text(..), intersperse)
import qualified Data.Text as Text
import Data.List.Split

area :: Box -> Int
area (Box l w h) = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]

data Box = Box Int Int Int
  deriving (Show, Eq)

mkBox [l,w,h] = Just $ Box (read l) (read w) (read h)
mkBox _ = Nothing


main =  fmap (sum . fmap area) . sequence . init . map (mkBox . splitOn "x") . splitOn "\n" <$> getContents >>= print
