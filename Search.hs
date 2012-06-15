module Search where

import Control.Parallel.Strategies

import qualified Data.List as L

type Result = [(Int,Int,Int)]
type Solver = [Int] -> Int -> [Result]

compute :: Solver
compute t s = let st = L.tails $ reverse $  L.sort t in
  parMap (rparWith rdeepseq) (snd . compute' 0 0 s s [] []) st

  where
  compute' _ _ _ _ b l [] = (l, b)
  compute' x y w h b l (q:qs)
    | w <= 0 || h <= 0 = ((q:qs) ++ l, b)
    | q > w || q > h = compute' x y w h b (q:l) qs
    | otherwise =
      let
        r = (x, y, q):b
        (ld, rd) = compute' (x+q) (y+q) (w-q) (h-q) r [] (l ++ qs)
        (lt, rt) = compute' x (y+q) q (h-q) rd [] ld
      in
        compute' (x+q) y (w-q) q rt [] lt

-- rdtr

rbrtCompute :: Solver
rbrtCompute ts s = parMap (rparWith rdeepseq)  (\tts -> fst $ rc 0 0 s s [] [] tts) $ L.tails $ L.sortBy (flip compare) ts

move :: [a] -> [a] -> [a]
move [] to = to
move (x:from) to = move from $ x:to

rc x y w h rs' sts []         = (rs',sts)
rc x y w h rs' sts ats@(t:ts) | w <= 0 || h <= 0 = (rs', move ats sts)
                              | t > min w h = rc x y w h rs' (t:sts) ts
                              | otherwise =
                                let
                                  rs = (x,y,t): rs'
                                  (rd,sd) = rc (x+t) (y+t) (w-t) (h-t) rs [] ts
                                  (rt,st) = rc x (y+t) t (h-t) rd [] $ reverse sd
                                  (rr,sr) = rc (x+t) y (w-t) t rt [] $ reverse st
                                in
                                  (rr,sr ++ sts)

{-

rbrtCompute [3,3,2,2,2,2,2] 6

Ergebnis:
...333
22.333
22.333
333...
33322.
33322.

Besser:
222222
222222
....22
333.22
333.22
333.22

-}
