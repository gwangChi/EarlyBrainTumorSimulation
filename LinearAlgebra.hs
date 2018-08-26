-- LINEAR ALGEBRA MODULE
module LinearAlgebra
   (
      Coord_X
    , Coord_Y
    , Coord_Z
    , Coord
    , Coord2
    , Vector
    , Matrix
    , eye
    , dist2
    --, dist3
    , length2
    , length3
    , multMV
    , multDM
    , multDV
    , multMM
    , mult2
    , mult3
    , plus3
    , plusV3
    , sbtr2
    , sbtr3
    , map3
    , sign
    --, abs
    , (<+>)
    , (<->)
    , (<**>)
    , showPrettyCoordss
    , showPrettyCoords
    , showPrettyDoubless
    , showPrettyDoubles
    , showPrettyVectorss
    , showPrettyVectors
    , doubles2Vectors
    , doubless2Vectorss
    , avgMagss2Mags
    , sumMagss2Mags
    , getTransNorm
    , duplicateVector
    , sumSineThetas
    , sumThruThetas
    , avgThruThetas
   ) where

type Coord_X = Double
type Coord_Y = Double
type Coord_Z = Double
type Coord = (Coord_X, Coord_Y, Coord_Z)
type Vector = (Double, Double, Double)
type Matrix = (Vector, Vector, Vector)
eye :: Matrix
eye = ((1,0,0),(0,1,0),(0,0,1))
type Coord2 = (Coord_X, Coord_Y)

multMV :: Matrix -> Vector -> Vector
multMV ((a,b,c),(d,e,f),(g,h,i)) (x,y,z) = (a*x+d*y+g*z, b*x+e*y+h*z, c*x+f*y+i*z)

multDM :: Double -> Matrix -> Matrix
multDM x ((a,b,c),(d,e,f),(g,h,i)) = ((a*x,b*x,c*x),(d*x,e*x,f*x),(g*x,h*x,i*x))

multDV :: Double -> Vector -> Vector
multDV a (x,y,z) = (a*x,a*y,a*z)

divDV :: Double -> Vector -> Vector
divDV a (x,y,z) = (x/a,y/a,z/a)

multMM :: Matrix -> Matrix -> Matrix
multMM ((a1,b1,c1),(d1,e1,f1),(g1,h1,i1)) ((a2,b2,c2),(d2,e2,f2),(g2,h2,i2))
     = ((a1*a2+d1*b2+g1*c2, b1*a2+e1*b2+h1*c2, c1*a2+f1*b2+i1*c2),
        (a1*d2+d1*e2+g1*f2, b1*d2+e1*e2+h1*f2, c1*d2+f1*e2+i1*f2),
        (a1*g2+d1*h2+g1*i2, b1*g2+e1*h2+h1*i2, c1*g2+f1*h2+i1*i2))

mult2 :: Double -> Coord2 -> Coord2
mult2 a (x,y) = (a*x, a*y)

mult3 :: Double -> Coord -> Coord
mult3 a (x,y,z) = (a*x, a*y, a*z)

plus3 :: Coord -> Coord -> Coord
plus3 (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

dist2 :: Coord2 -> Coord2 -> Double
dist2 (x1,y1) (x2,y2) = sqrt ((x1-x2)**2+(y1-y2)**2)

length2 :: Coord2 -> Double
length2 (x,y) = sqrt (x**2+y**2)

length3 :: Coord -> Double
length3 (x, y, z) = sqrt (x**2+y**2+z**2)

sbtr2 :: Coord2 -> Coord2 -> Coord2
sbtr2 (x1, y1) (x2, y2) = (x1-x2, y1-y2)

sbtr3 :: Coord -> Coord -> Coord
sbtr3 (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

map3 :: (Double->Double) -> Coord -> Coord
map3 f (x,y,z) = (f x, f y, f z)

sign :: Double -> Double
sign x | x<0 = -1
       | x>0 = 1
       | otherwise = 0

--abs :: Double -> Double
--abs x | x<0 = -x
--      | otherwise = x

(<+>) :: Matrix -> Matrix -> Matrix
((a1,b1,c1),(d1,e1,f1),(g1,h1,i1)) <+> ((a2,b2,c2),(d2,e2,f2),(g2,h2,i2)) = ((a1+a2,b1+b2,c1+c2),(d1+d2,e1+e2,f1+f2),(g1+g2,h1+h2,i1+i2))

(<->) :: Matrix -> Matrix -> Matrix
m1 <-> m2 = m1 <+> multDM (-1) m2

(<**>) :: Double -> Matrix -> Matrix
a <**> m1 = multDM a m1

showPrettyCoordss :: [[Coord]] -> String
showPrettyCoordss xss = foldr addLine "" (map showPrettyCoords xss)
                        where addLine xs ys = xs++"\n"++ys

showPrettyCoords :: [Coord] -> String
showPrettyCoords xs = foldr addBlank "" (map show xs)
                      where addBlank x y = x++" "++y

showPrettyDoubless :: [[Double]] -> String
showPrettyDoubless xss = foldr addLine "" (map showPrettyDoubles xss)
                         where addLine xs ys = xs++"\n"++ys

showPrettyDoubles :: [Double] -> String
showPrettyDoubles xs = foldr addBlank "" (map show xs)
                       where addBlank x y = x++" "++y

showPrettyVectorss :: [[Vector]] -> String
showPrettyVectorss xss = foldr addLine "" (map showPrettyVectors xss)
                        where addLine xs ys = xs++"\n"++ys

showPrettyVectors :: [Vector] -> String
showPrettyVectors xs = foldr addBlank "" (map show xs)
                      where addBlank x y = x++" "++y

doubless2Vectorss :: [[Double]] -> [[Vector]]
doubless2Vectorss xss = map doubles2Vectors xss

doubles2Vectors :: [Double] -> [Vector]
doubles2Vectors [] = []
doubles2Vectors (x:xs) = [(0,0,x)]++doubles2Vectors xs

plusV3 :: Vector -> Vector -> Vector
plusV3 (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

avgMagss2Mags :: [[Vector]] -> [Vector]
avgMagss2Mags vss = map (divDV (fromIntegral (length vss))) (sumMagss2Mags vss)

sumMagss2Mags :: [[Vector]] -> [Vector]
sumMagss2Mags [vs] = vs
sumMagss2Mags (vs:vss) = zipWith (plusV3) vs (sumMagss2Mags vss)

getTransNorm :: Vector -> Double
getTransNorm = \(x,y,z) -> sqrt (x**2+y**2)

duplicateVector :: Int -> Vector -> [Vector]
duplicateVector 0 _ = []
duplicateVector n v = [v] ++ duplicateVector (n-1) v

sumSineThetas :: [Double] -> Double
sumSineThetas [] = 0
sumSineThetas (x:xs) = sin x + sumSineThetas xs

sumThruThetas :: [Double] -> [[Vector]] -> [Vector]
sumThruThetas [theta] [vs] = map (multDV (sin theta)) vs
sumThruThetas (theta:thetas) (vs:vss) = zipWith (plusV3) (map (multDV (sin theta)) vs) (sumThruThetas thetas vss)

avgThruThetas :: [Double] -> [[Vector]] -> [Vector]
avgThruThetas thetas vss = map (divDV (sumSineThetas thetas)) (sumThruThetas thetas vss)