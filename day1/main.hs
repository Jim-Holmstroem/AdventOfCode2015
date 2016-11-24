import Control.Applicative

santa :: [Char] -> Int
santa [] = 0
santa ('(':xs) = santa xs + 1
santa (')':xs) = santa xs - 1


main :: IO ()
main = santa <$> getContents >>= putStrLn . show
