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
xor_1 a b
-- xor_1 a _ = error "Mal"
   | True  || True  =  not False
   | True  || False =  not True
   | False || True  =  not True
   | False || False =  not False

--ej 6.3

xor_3 :: Bool -> Bool -> Bool
xor_3 a b = (a || b) && not (a && b)

-- Ejercicio 6.4.
-- Funcion de orden superior
xor_4 :: Bool -> Bool -> Bool
xor_4 a b = a /= b

-- Ejercicio 7
-- ultimos segun indica s
finales :: Int -> [a] -> [a]
finales s xs = drop (length xs - s) xs

-- Ejercicio 8
-- Funcion de eliminacion Drop y take
-- Dos funciones en un mismo ejecucion como lo hace lambda 
segmento :: Int -> Int -> [a] -> [a]
-- drop se comporta de atras adelan
-- take toma las primeras posiciones
segmento o p xs = drop(o-1) (take p xs)

-- Uso de funcion max y min de Haskell

mediano :: (Num a, Ord a) => a -> a -> a -> a
mediano x y z = x + y + z - minimum [x,y,z] - maximum [x,y,z]

-- Ejercicio 10. 
-- distancia :: (Floating a) => a1 -> b1 -> a2 -> b2 -> a  
distancia x1 y1 x2 y2 = sqrt((x1-x2)^2 + (y1-y2)^2)

-- Ejercicio 11.
extremos n xs = take n xs ++ drop (length xs - n) xs

-- Ejercicio 12.
puntoMedio :: (Fractional a, Fractional b) => (a, b) -> (a, b) -> (a, b)
puntoMedio (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

-- Ejercicio 13.
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs

-- Ejercicio 14.
numeroMayor :: (Num a, Ord a) => t -> t1 -> a
numeroMayor u x = u*10 + x
      where u = max u x
            x = min u x
-- ejecicio 15.
triangular :: (Num a, Ord a) => a -> a -> a -> Bool
triangular a b c = a < b+c && b < a+c && c < a+b