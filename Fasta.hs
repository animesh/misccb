module Main where
import Data.List (partition)

main = do
   xs <- readFile "fasta.seq"
   print (count xs)

count str = let (cg,at) = partition (\x->x=='c'||x=='g') . concat . filter ((/='>').head) . lines $ str
            in fromIntegral (length cg) / fromIntegral (length cg+length at)
