yopoYepe xs = [if x < 10 then "yopo" else "yepe" | x <- xs, odd x]
length' xt = sum[1|_ <- xt]
-- Cuestion de tipos
removeLower :: [Char] -> [Char]
removeLower st = [c | c <- st, c `elem` ['a'..'z']]
-- Con valores no tan grandes
addTrue :: Int -> Int -> Int -> Int
addTrue x y z = x + y * z
-- Valores grandes de enteros
addTrues :: Integer -> Integer -> Integer -> Integer
addTrues a b c = a * b * c

factoryal :: Integer -> Integer
factoryal f = product [1..f]


-- Números de Punto Flotante

-- Float numero con simple precisión
circunfe :: Float -> Float
circunfe z = 2 * pi * z
-- Double doble precisión :)
circunfe' :: Double -> Double
circunfe' r = 2 * pi * r

-- Funciones
luck :: (Integral c) => c -> String
luck 8 = "Que bien compa"
luck x = "Mal Compa" 

lucky :: (Integral a) => a -> String
lucky 1 = "Uno!" 
lucky 2 = "Dos!"
lucky 3 = "Tres!"
lucky 4 = "Cuatro!"
lucky 5 = "Cinco!"
lucky x = "NO! NO! solo de 1 a 5"

factorial :: (Integral t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n -1)

-- Ajuste de patron "Pattern Matching"
sumaVector :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumaVector a b = (fst a + fst b, snd a + snd b)

sumaVectores :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumaVectores (x1, y1) (x2, y2 ) = (x1 + x2, y1 + y2)

-- Primeros elementos de una lista
tell :: (Show a) => [a] -> String
tell []       = "La lista está vacía"
tell (x:[])   = "La lista tiene un elemento: " ++ show x
tell (x:y:[]) = "La lista tiene dos elementos: " ++ show x ++ " y " ++ show y
tell (x:y:_)  = "La lista es larga. Los primeros dos elementos son: " ++ show x ++ " y " ++ show y

-- patrones como, o patrones as (del inglés, as patterns)
capitalman :: String -> String
capitalman "" = "Nada vacio compa!"
capitalman all@(x:_) = "La primera letra de "++ all ++ " es " ++ [x]

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- Guardas 
pesos :: (RealFloat a) => a -> String
pesos peso 
    | peso <= 18.5 = "Eres muy bajo de Peso, emo"
    | peso <= 25.0 = "Eres normal"
    | peso <= 30.0 = "Estas subiendo de Peso mucho"
    | otherwise    = "Estoy gordo, estoy gordo de Big Poppa"

pesoDos :: (RealFloat a) => a -> a -> String
pesoDos w h
    | w / h ^ 2 <= 18.5 = "Eres muy bajo de Peso, emo"
    | w / h ^ 2 <= 25.0 = "Eres normal"
    | w / h ^ 2 <= 30.0 = "Estas subiendo de Peso mucho" 
    | otherwise         = "Estoy gordo, estoy gordo de Big Poppa"

comparar :: (Ord a) => a -> a -> Ordering
a `comparar` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- Principio DRY

pesoTres :: (RealFloat q) => q -> q -> String
pesoTres w h
    | peso <= 18.5 = "Eres muy bajo de Peso, emo"
    | peso <= 25.0 = "Eres normal"
    | peso <= 30.0 = "Estas subiendo de Peso mucho"
    | otherwise    = "Estoy gordo, estoy gordo de Big Poppa"
-- Where es construcción sintáctica
    where peso = w / h ^ 2

pesoCuatro :: (RealFloat q) => q -> q -> String
pesoCuatro w h
    | peso <= flaco  = "Eres muy bajo de Peso, emo"
    | peso <= normal = "Eres normal"
    | peso <= gordo  = "Estas subiendo de Peso mucho"
    | otherwise      = "Estoy gordo, estoy gordo de Big Poppa"
    where peso = w / h ^ 2
          (flaco, normal, gordo) = (18.5, 25.0, 30.0)

cilindro :: (RealFloat s) => s -> s -> s
cilindro r h =
-- Let son expresiones por si misma
    let ladoArea = 2 * pi * r * h
        arribaArea = pi * r ^ 2
    in  ladoArea + 2 * arribaArea

-- Usando el Case como en imperativos
describeLista :: [a] -> String
describeLista es = "La lista es" ++ case es of []   -> "una lista vacía."
                                               [x]   -> "una lista unitaria."
                                               es    -> "una lista larga."  
