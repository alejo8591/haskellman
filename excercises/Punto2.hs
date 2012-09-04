-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función modulo tal que (modulo v) es el
-- módulo del vector v. Por ejemplo,
-- modulo (3,4) == 5.0
-- ---------------------------------------------------------------------
modulo :: (Floating x) => x -> x -> x
modulo x y  = sqrt(x^2 + y^2)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función cuadrante tal que (cuadrante p) es
-- es cuadrante del punto p (se supone que p no está sobre los
-- ejes). Por ejemplo,
-- cuadrante (3,5) == 1
-- cuadrante (-3,5) == 2
-- cuadrante (-3,-5) == 3
-- cuadrante (3,-5) == 4
-- ---------------------------------------------------------------------
-- Utilizando cerraduras
cuadrante x y
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función intercambia tal que (intercambia p)
-- es el punto obtenido intercambiando las coordenadas del punto p. Por
-- ejemplo,
-- intercambia (2,5) == (5,2)
-- intercambia (5,2) == (2,5)
-- ---------------------------------------------------------------------
-- Propiedad de las tuplas
intercambia (x,y) = (y,x)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función simetricoH tal que (simetricoH p) es
-- el punto simétrico de p respecto del eje horizontal. Por ejemplo,
-- simetricoH (2,5) == (2,-5)
-- simetricoH (2,-5) == (2,5)
-- ---------------------------------------------------------------------

simetricoH (x,y) = (x,(y*(-1)))

-- ---------------------------------------------------------------------
-- Ejercicio 5. (Raíces de una ecuación de segundo grado) Definir la
-- función raices de forma que (raices a b c) devuelve la lista de las
-- raices reales de la ecuación ax^2 + bx + c = 0. Por ejemplo,
-- raices 1 (-2) 1 == [1.0,1.0]
-- raices 1 3 2 == [-1.0,-2.0]
-- ---------------------------------------------------------------------
-- raices :: Integer -> Floating -> Floating -> Floating
raices a b c
    | d >= 0 = [(-b+e)/(2*a), (-b-e)/(2*a)]
    | otherwise = error "No tine raices reales"
      where d = b^2-(4 * a * c)
            e = sqrt d

-- ---------------------------------------------------------------------
-- Ejercicio 6. La disyunción excluyente xor de dos fórmulas se verifica
-- si una es verdadera y la otra es falsa.
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función xor_1 que calcule la disyunción
-- excluyente a partir de la tabla de verdad. Usar 4 ecuaciones, una por
-- cada línea de la tabla.
-- ---------------------------------------------------------------------

xor_1 :: Bool -> Bool -> Bool
xor_1 True True = False
xor_1 True False = True
xor_1 False True = True
xor_1 False False = False

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función xor_3 que calcule la disyunción
-- excluyente a partir de la disyunción (||), conjunción (&&) y negación
-- (not). Usar 1 ecuación.
-- ---------------------------------------------------------------------


xor_3 :: Bool -> Bool -> Bool
xor_3 a b = (a || b) && not (a && b)

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la función xor_4 que calcule la disyunción
-- excluyente a partir de desigualdad (/=). Usar 1 ecuación.
-- ---------------------------------------------------------------------

-- Funcion de orden superior
xor_4 :: Bool -> Bool -> Bool
xor_4 a b = a /= b


-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función finales tal que (finales n xs) es la
-- lista formada por los n finales elementos de xs. Por ejemplo,
-- finales 3 [2,5,4,7,9,6] == [7,9,6]
-- ---------------------------------------------------------------------

finales s xs = drop (length xs - s) xs

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y
-- n. Por ejemplo,
-- segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
-- segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
-- segmento 5 3 [3,4,1,2,7,9,0] == []
-- ---------------------------------------------------------------------
-- Funcion de eliminacion Drop y take

segmento o p xs = drop (o-1) (take p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función mediano tal que (mediano x y z) es el
-- número mediano de los tres números x, y y z. Por ejemplo,
-- mediano 3 2 5 == 3
-- mediano 2 4 5 == 4
-- mediano 2 6 5 == 5
-- mediano 2 6 6 == 6
-- ---------------------------------------------------------------------

-- Uso de funcion max y min de Haskell

mediano x y z = x + y + z- minimum [x,y,z] - maximum [x,y,z]

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función distancia tal que (distancia p1 p2)
-- es la distancia entre los puntos p1 y p2. Por ejemplo,
-- distancia (1,2) (4,6) == 5.0
-- ---------------------------------------------------------------------

-- Trabajando con duplas

distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+ (y1-y2)^2)

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs. Por ejemplo,
-- extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
-- ---------------------------------------------------------------------

extremos n xs = take n xs ++ drop (length xs - n) xs

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función puntoMedio tal que (puntoMedio p1 p2)
-- es el punto medio entre los puntos p1 y p2. Por ejemplo,
-- puntoMedio (0,2) (0,6) == (0.0,4.0)
-- puntoMedio (-1,2) (7,6) == (3.0,4.0)
-- ---------------------------------------------------------------------

puntoMedio (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir una función ciclo que permute cíclicamente los
-- elementos de una lista, pasando el último elemento al principio de la
-- lista. Por ejemplo,
-- ciclo [2, 5, 7, 9] == [9,2,5,7]
-- ciclo [] == [9,2,5,7]
-- ciclo [2] == [2]
-- ---------------------------------------------------------------------

ciclo [] = []

ciclo xs = last xs : init xs

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la funcion numeroMayor tal que
-- (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,
-- numeroMayor 2 5 == 52
-- numeroMayor 5 2 == 52
-- ---------------------------------------------------------------------

numeroMayor u x = u*10 + x
      where u = max u x
            x = min u x

-- ---------------------------------------------------------------------
-- Ejercicio 15. Las longitudes de los lados de un triángulo no pueden
-- ser cualesquiera. Para que pueda construirse el triángulo, tiene que
-- cumplirse la propiedad triangular; es decir, longitud de cada lado
-- tiene que ser menor que la suma de los otros dos lados.
--
-- Definir la función triangular tal que (triangular a b c) se verifica
-- si a, b y c complen la propiedad triangular. Por ejemplo,
-- triangular 3 4 5 == True
-- triangular 30 4 5 == False
-- triangular 3 40 5 == False
-- triangular 3 4 50 == False
-- ---------------------------------------------------------------------
triangular a b c = a < b+c && b < a+c && c < a+b
