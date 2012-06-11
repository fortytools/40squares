module Main where

import Control.Monad

import qualified Data.List as L

main :: IO ()
main = do
  tiles <- getLine
  size <- getLine

  results <- return $ compute (read tiles) (read size)

  return ()

compute :: [Int] -> Int -> [[(Int, Int, Int)]]
compute t s = let st = tails $ reverse $ L.sort t in
  undefined

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])
