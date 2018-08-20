module Tema1 (
        solveSimple,
        solveCosts
        ) where
import Data.Array

import Data.Array

unu (a, b, c) = a
doi (a, b, c) = b
trei (a, b, c) = c

fun i per = if i == unu per  || i == doi per then True
             else False

cauta lista i = filter (fun i) lista

noLast [] = []
noLast (a : []) = []
noLast (a : xs) = a : (noLast xs)

gen 0 = []
gen n = (gen (n - 1)) ++ (n : [])

co l1 l2 = map (cauta l2) l1
trans l = listArray (0, (length l) - 1) l

ad n l = co (gen n) l

solve (n, e) = floyd 1 n n where
        floyd i j 0 = if i == j then ((i : []), 0)
                      else if (cauta (lista !! (i - 1)) j) /= [] then ((i : (j : [])), trei (head (cauta (lista !! (i - 1)) j)))
                      else ([], maxInt)

        floyd i j k = if snd (dp ! (i, j, k - 1)) ==  maxInt && snd (dp ! (i, k, k - 1)) ==  maxInt then  ([], maxInt)
                      else if snd (dp ! (i, j, k - 1)) ==  maxInt && snd (dp ! (k, j, k - 1)) ==  maxInt then  ([], maxInt)
                      else if snd (dp ! (i, j, k - 1)) ==  maxInt then ((fst(dp ! (i, k, k - 1))) ++ tail(fst(dp ! (k, j, k - 1))), snd (dp ! (i, k, k - 1)) + snd (dp ! (k, j, k - 1)))
                      else if snd (dp ! (i, k, k - 1)) ==  maxInt || snd (dp ! (k, j, k - 1)) == maxInt then (dp ! (i, j, k - 1))
                      else if snd (dp ! (i, j, k - 1)) > snd (dp ! (i, k, k - 1)) + snd (dp ! (k, j, k - 1))
                                then ((fst(dp ! (i, k, k - 1))) ++ tail(fst(dp ! (k, j, k - 1))), snd (dp ! (i, k, k - 1)) + snd (dp ! (k, j, k - 1)))
                       else dp ! (i, j, k - 1)
        maxInt = 1000000000
        lista = ad n e
        dp = listArray ((1, 1, 0), (n, n, n)) [floyd x y z | (x, y, z) <- range ((1,1,0), (n, n,n))]

solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple (n, e) = case solve (n, e) of
                     ([], 1000000000) -> Nothing
                     (a, b) -> Just (a, b)



getNodes i [] = []
getNodes i (x : xs) = if i == unu x then ((doi x, trei x) : (getNodes i xs))
                      else ((unu x, trei x) : (getNodes i xs))

compute s dp c (a, b) = let maxInt = 1000000000 in 
                        if s < c ! (a - 1) then (maxInt, -1)
                        else if snd (dp ! (a, s - (c ! (a - 1)))) == maxInt then (maxInt, - 1)
                        else (b + snd (dp ! (a, s - (c ! (a - 1)))), snd (last(fst(dp ! (a, s - (c ! (a - 1)))))))

com x y = if fst x > fst y then True
              else if fst x == fst y && snd x < snd y then True
              else False

mini s dp c [] min indx  = (indx, min)
mini s dp c (x : xs) min indx  = if com min (compute s dp c x) == True then (mini s dp c xs (compute s dp c x) (fst x))
                                 else (mini s dp c xs min indx)

solve1 (n, m, c, e) = din 1 m where
        din nod s = if s < 0 then ([], maxInt)
                    else if nod == n then (((nod, s) : []), 0)
                    else case mini s dp c1 (getNodes nod (lista1 ! (nod - 1))) (maxInt, - 1) (-1) of
                         (a, (b, o)) -> if b == maxInt then ([], maxInt)
                                        else (((nod , s) : fst(dp ! (a, s - (c1 ! (a - 1))))), b)
        maxInt = 1000000000
        lista1 = listArray (0, n - 1) lista
        lista = ad n e
        c1 = trans c
        dp = listArray ((1, 0), (n, m)) [din x y | (x , y) <- range ((1, 0), (n, m))]

solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts (n, m, c, e) = case solve1 (n, m, c, e) of
                          (_, 1000000000) -> Nothing
                          (a, b) -> Just (a, b)
