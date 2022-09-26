-- Tievoli Bruno
-- Riso Santiago
-- Gozzi Juan

-- EJERCICIO 1. sonCoprimos
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = maximoComunDivisor a b == 1

-- EJERCICIO 2. es2PseudoPrimo
es2PseudoPrimo :: Integer -> Bool
es2PseudoPrimo n = esAPseudoPrimo 2 n

-- EJERCICIO 3. cantidad3PseudoPrimos
cantidad3PseudoPrimos :: Integer -> Integer
cantidad3PseudoPrimos n = contador3PseudoPrimos n 0

-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo n = contadorKEsimo2y3Pseudoprimo n 2 1

-- EJERCICIO 5: esCarmichael
esCarmichael :: Integer -> Bool
esCarmichael n = contadorCarmichael n 1

-----------------------------------------------------------------------
-- Funciones auxiliares
maximoComunDivisor :: Integer -> Integer -> Integer
maximoComunDivisor a 0 = a
maximoComunDivisor a b = maximoComunDivisor b (a `mod` b)

menorDivisor :: Integer -> Integer -> Integer
menorDivisor 1 _ = 1
menorDivisor n m | n `mod` m == 0 = m
                 | otherwise = menorDivisor n (m+1) 

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n 2 == n

esAPseudoPrimo :: Integer -> Integer -> Bool
esAPseudoPrimo a n = (a^(n-1)-1) `mod` n == 0 && not (esPrimo n)

es3PseudoPrimo :: Integer -> Bool
es3PseudoPrimo n = esAPseudoPrimo 3 n

contador3PseudoPrimos :: Integer -> Integer -> Integer
contador3PseudoPrimos 1 m = m
contador3PseudoPrimos n m | es3PseudoPrimo n = contador3PseudoPrimos (n-1) (m+1)
                          | otherwise = contador3PseudoPrimos (n-1) m
                          

contadorKEsimo2y3Pseudoprimo :: Integer -> Integer -> Integer -> Integer
contadorKEsimo2y3Pseudoprimo 0 m k = k
contadorKEsimo2y3Pseudoprimo n m k | es2PseudoPrimo m && es3PseudoPrimo m = contadorKEsimo2y3Pseudoprimo (n-1) (m+1) m
                                   | otherwise = contadorKEsimo2y3Pseudoprimo n (m+1) k
                                   

contadorCarmichael :: Integer -> Integer -> Bool
contadorCarmichael n m | m == n = True
                       | sonCoprimos m n && not (esAPseudoPrimo m n) = False
                       | otherwise = contadorCarmichael n (m+1)

