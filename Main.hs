module Main (main) where

import Encoder
import System.Random

main :: IO ()
main = do
  g <- getStdGen
  putStrLn (show (createCipher ['a' .. 'z'] g))
