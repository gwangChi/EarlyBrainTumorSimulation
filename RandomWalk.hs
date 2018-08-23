-- RANDOM WALK MODULE
module RandomWalk
   (
      isTrespass
    , randStep
    , checkStep
    , checkPeriod
    , randWalk
    , initCoord
   ) where

-- IMPORTS THE RANDOM NUMBER LIBRARY
import System.Random
import LinearAlgebra

isTrespass :: Double -> [Coord2] -> [Coord2] -> Bool
isTrespass r [] [] = False
isTrespass r (x1:xs1) (x2:xs2) = not (r < length2 x1 && r < length2 x2
                              || r >= length2 x1 && r >= length2 x2) || isTrespass r xs1 xs2 

checkStep :: Double -> Double -> Double -> Coord -> Coord -> Bool
checkStep num_cyl r_cyl0 l_cube (x1,y1,_) (x2,y2,_) = not (isTrespass (r_cyl0/sqrt num_cyl) (choose num_cyl (x1,y1)) (choose num_cyl (x2,y2)))
   where choose :: Double -> Coord2 -> [Coord2]
         choose n coord | n==1 = sbtr2 <$> [coord] <*> map (mult2 l_cube) [(0,0)]
                        | n==2 = sbtr2 <$> [coord] <*> map (mult2 l_cube) [(-1/4,-1/4),(1/4,1/4)]
                        | n==4 = sbtr2 <$> [coord] <*> map (mult2 l_cube) [(-1/4,-1/4),(1/4,-1/4),(1/4,1/4),(-1/4,1/4)]
                        | n==9 = sbtr2 <$> [coord] <*> map (mult2 l_cube) [(-1/3,-1/3),(0,-1/3),(1/3,-1/3),(1/3,0),(1/3,1/3),(0,1/3),(-1/3,1/3),(-1/3,0),(0,0)]

checkPeriod :: Double -> Double -> Double
checkPeriod l_cube x | x < (-l_cube/2) = x + l_cube
                     | x > (l_cube/2) = x - l_cube
                     | otherwise = x
{--
checkPeriod :: Double -> Double -> Double
checkPeriod l_cube x | x < (-l_cube/2) = checkPeriod l_cube (x + l_cube)
                     | x > (l_cube/2) = checkPeriod l_cube (x - l_cube)
                     | otherwise = x
--}
newRand = randomIO :: IO Double

-- http://mathworld.wolfram.com/SpherePointPicking.html
randStep :: IO (Coord)
randStep = do
   u <- newRand
   v <- newRand
   return (sin (2*pi*u)*(2*v-1), sin (2*pi*u)*sqrt (1-(2*v-1)**2), sqrt (1-sin (2*pi*u)**2))

randWalk :: Int -> Double -> Double -> Double -> Double -> Coord -> IO ([Coord])
randWalk curr num_cyl r_cyl0 l_cube l_step coord1 = do
   vstep <- randStep
   --putStrLn (show vstep)
   let coord2 = map3 (checkPeriod l_cube) (plus3 coord1 (mult3 l_step vstep))
   putStrLn (show coord2)
   if checkStep num_cyl r_cyl0 l_cube coord1 coord2 && curr == 1 then
      return [coord2]
   else if checkStep num_cyl r_cyl0 l_cube coord1 coord2 then do
      steps <- randWalk (curr-1) num_cyl r_cyl0 l_cube l_step coord2
      return ([coord2] ++ steps)
   else do
      putStrLn " Reset "
      randWalk curr num_cyl r_cyl0 l_cube l_step coord1

initCoord :: Int -> Double -> IO ([Coord])
initCoord 1 l_cube = do
   u <- newRand
   v <- newRand
   w <- newRand
   return ([mult3 l_cube (u-0.5,v-0.5,w-0.5)])
initCoord n l_cube = do
   u <- newRand
   v <- newRand
   w <- newRand
   coords <- initCoord (n-1) l_cube
   return ([mult3 l_cube (u-0.5,v-0.5,w-0.5)]++coords)






