module Main (main) where

import Encoder
import System.Random
import System.Environment
import Data.Set (fromList)

main :: IO ()
main = do 
  args <- getArgs
  case args of
    []          -> putStrLn "No input file given."
    (infile:as) -> do 
      fcontents <- readFile infile 
      g <- getStdGen
      writeFile (infile ++ ".ciph") (ec g fcontents) where
        ec gen str = enCipher (createCipher (fromList ['a' .. 'z']) gen) str 
