promedio :: (Integral n) => n -> n
promedio x y z = (x + y + z)/3
main = do
-- juntando varias opciones IO()
    putStrLn "Escriba su número de Cedula"
    cc <- getLine
    putStrLn "Escriba la primera nota"
    num1 <- getLine
    putStrLn "Escriba la segunda nota"
    num2 <- getLine
    putStrLn "Escriba la tercera nota"
    num3 <- getLine
    promedio num1 num2 num3
    putStrLn ("Su número de Cedula es " ++ cc)
