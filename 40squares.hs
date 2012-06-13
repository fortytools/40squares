module Main where

import System.IO
import System.Exit
import System.Environment

import Search

main :: IO ()
main = do
  args <- getArgs
  debug <- return $ "--debug" `elem` args

  tiles <- getLine
  size <- getLine

  tiles' <- return $ read tiles
  size' <- return $ read size

  if debug then do
    putStrLn $ "tiles: " ++ show tiles' ++ " size: " ++ show size'
    else return ()

  -- GO GO GO!
  results <- return $ (onlyBetter 0) $ compute tiles' size'
  mapM_ (flushPrint debug) results

  if debug  then do
    available <- return $ foldr (\t r -> r + t * t) 0 tiles'
    target <- return $ size' * size'
    best <- return $ score $ last results
    putStrLn $ "avail : " ++ (show available)
    putStrLn $ "target: " ++ (show target)
    putStrLn $ "best  : " ++ (show best)
    putStrLn $ "missed: " ++ (show $ (min available target) - best)
    putStrLn $ "results: " ++ (show $ length results)
    else return ()

  exitWith ExitSuccess

  where
  flushPrint d s = do
    if d then
      putStrLn $ show (score s) ++ " " ++ (show $ length s) ++ " " ++ (show s)
    else putStrLn $ show s

    hFlush stdout
  onlyBetter _ [] = []
  onlyBetter m (s:l) = let p = score s in
    if p > m then  s : onlyBetter p l else onlyBetter m l

score :: [(Int, Int, Int)] -> Int
score [] = 0
score ((_, _, s):ss) = s*s + score ss

