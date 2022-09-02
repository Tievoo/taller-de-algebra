absoluto x = sqrt (x**2)
 
maximoabsoluto x y = maximo (abs x) (abs y)
 
maximo3 x y z = maximo (maximo x y) z

algunoEs0 x y = x == 0 || y == 0
 
ambosSon0 x y = x == 0 && y == 0
 
esMultiploDe x y = mod x y == 0
 
digitoUnidades x = mod x 10
 
digitoDecenas x = div (mod x 100) 10