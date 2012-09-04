-- ej 1
modulo x y  = sqrt(x^2 + y^2)

-- ejercicio con patrones
modulo' :: (Floating a) => a -> a -> a
modulo' x y = sqrt(x^2 + y^2)
-- ej 2
-- Utilizando cerraduras
cuadrante :: (Num a1, Num a2, Num a, Ord a1, Ord a2) => a1 -> a2 -> a
cuadrante x y
    | c && d = 1
    | a && d = 2
    | a && b = 3
    | c && b = 4
    | otherwise = error "Valor no esperado"
    where
          a = x < 0
          b = y < 0
          c = x > 0
          d = y > 0

-- ej 3
-- con tuplas
intercambia :: (a, b) -> (b, a)
intercambia (x,y) = (y,x)
-- sin tuplas al inicio y arrojando tupla
intercambia' :: (Num a, Num b) => a -> b -> (b,a) 
intercambia' x y = (y, x)

-- ej 4
simetricoH :: (Num a, Num b) => a -> b -> (a, b)
simetricoH x y = (x,(y*(-1)))

-- ej 5
raices :: (Floating a, Ord a) => a -> a -> a -> [a]
raices a b c
    | d >= 0 = [(-b+e)/(2*a), (-b-e)/(2*a)]
    | otherwise = error "Este caso No tine raices reales"
      where d = b^2-(4 * a * c)
            e = sqrt d
--ej 6
xor_1 :: Bool -> Bool -> Bool
xor_1 True True = False
xor_1 True False = True
xor_1 False True = True
xor_1 False False = False