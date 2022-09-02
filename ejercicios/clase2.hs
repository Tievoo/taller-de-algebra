range1 :: (Ord a, Num a) => a -> Bool
range1 x = x > 3 && x <=7
 
range2 :: (Ord a, Num a) => a -> Bool
range2 x = x <=3
 
range3 :: (Ord a, Num a) => a -> Bool
range3 x = x > 7
 
estanRelacionados :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> Bool
estanRelacionados a b = (range1 a && range1 b) || (range2 a && range2 b) || (range3 a && range3 b)
 
prodInt :: Num a => (a, a, a) -> (a, a, a) -> a
prodInt (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2
 
todoMenor :: (Ord a, Num a) => (a, a, a) -> (a, a, a) -> Bool
todoMenor (x1, y1, z1) (x2, y2, z2) = x1<x2 && y1<y2 && z1<z2
 
distanciaPuntos :: (Floating a) => (a,a) -> (a,a) -> a
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)
 
sumaTerna :: Num a => (a, a, a) -> a
sumaTerna (x,y,z) = x+y+z
 
posicPrimerPar :: Integral p => (p, p, p) -> p
posicPrimerPar (x,y,z) | even x = x
                       | even y = y
                       | even z = z
                       | otherwise = 4
 
crearPar :: a -> b -> (a, b)
crearPar x y = (x,y)
 
invertir :: (b, a) -> (a, b)
invertir (x,y) = (y,x)