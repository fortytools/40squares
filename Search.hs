module Search where

import Control.Parallel.Strategies

import qualified Data.List as L

compute :: [Int] -> Int -> [[(Int, Int, Int)]]
compute t s = let st = L.tails $ reverse $  L.sort t in
  parMap (rparWith rdeepseq) (snd . compute' 0 0 s s [] []) st

  where
  compute' _ _ _ _ b l [] = (l, b)
  compute' x y w h b l (q:qs)
    | w <= 0 || h <= 0 = ((q:qs) ++ l, b)
    | q > w || q > h = compute' x y w h b (q:l) qs
    | otherwise =
      let r = (x, y, q):b in
        let (ld, rd) = compute' (x+q) (y+q) (w-q) (h-q) r [] (l ++ qs) in
          let (lt, rt) = compute' x (y+q) q (h-q) rd [] ld in
            compute' (x+q) y (w-q) q rt [] lt

