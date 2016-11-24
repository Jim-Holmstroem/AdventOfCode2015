import Control.Monad
import Control.Applicative
import Data.List

walk [] = []
walk ('^':ss) = (1,0) : walk ss 
walk ('v':ss) = (-1,0) : walk ss 
walk ('>':ss) = (0,1) : walk ss 
walk ('<':ss) = (0,-1) : walk ss 

(a,b) .+ (c,d) = (a+c,b+d)

main = length . nub . scanl (.+) (0,0) . walk <$> getContents >>= print
