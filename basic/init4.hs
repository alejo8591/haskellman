-- stack overflow
fibonacci :: (Integral d) => d -> d
fibonacci d
    | d == 0 = 0
    | d == 1 = 1
    | otherwise = fibo
    where fibo = fibonacci d-1 + fibonacci d-2

maxi :: (Ord e) => [e] -> e
maxi [] = error "Lista vacía"
maxi [e] = e
maxi (e:ez)
    | e > maxVal = e
    | otherwise  = maxVal
    where maxVal = maxi ez
-- otra forma de recursion con funciones base de Haskell
maxi' :: (Ord h) => [h] -> h
maxi' []    =  error "Lista vacía"
maxi' [h]   =  h
maxi' (h:hs) =  h `max` (maxi' hs)
-- Jugando con la replicación
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0      = [] 
    | otherwise   = x:replicate' (n-1) x
-- Seguimos con recursion
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = [] 
take' n (x:xs) = x : take' (n-1) xs

-- reversar lista
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repetir elemento como resultado una lista
repeat' :: a -> [a]
repeat' s = s : repeat' s -- al infinito y más allá

-- función zip
zip' :: [a] -> [b] -> [(a,b)]
zip'  _ [] = []
zip' [] _  = []
zip' (h:hs) (u:us) = (h,u): zip' hs us

-- quicksort 
quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (c:cs) =
    let smallerSorted = quick [a | a <- cs, a <= c]
        biggerSorted  = quick [a | a <- cs, a > c]
    in  smallerSorted ++ [c] ++ biggerSorted

-- currificadas
-- aplicando concepto lambda
multTres :: (Num a) => a -> (a -> (a->(a->(a->a))))
multTres x y z a b = x * y * z * a * b
