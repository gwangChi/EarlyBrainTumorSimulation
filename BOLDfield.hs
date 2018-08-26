-- BOLD FIELD MODULE
module BOLDfield
   (
      getField
    , genCoordss2Field
    , coordss2Fieldss
   ) where

import LinearAlgebra
import RandomWalk

getField :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Coord -> Double
getField num_cyl theta chi y w0 r_cyl0 gamma l_cube coord
       = foldl (+) 0 (map (calcField theta chi y w0 (r_cyl0/(sqrt num_cyl)) gamma coord) (map (mult2 l_cube) (choose num_cyl)))
   where choose :: Double -> [Coord2]
         choose n | n==1 = [(0,0)]
                  | n==2 = [(-1/4,-1/4),(1/4,1/4)]
                  | n==4 = [(-1/4,-1/4),(1/4,-1/4),(1/4,1/4),(-1/4,1/4)]
                  | n==9 = [(-1/3,-1/3),(0,-1/3),(1/3,-1/3),(1/3,0),(1/3,1/3),(0,1/3),(-1/3,1/3),(-1/3,0),(0,0)]

calcField :: Double -> Double -> Double -> Double -> Double -> Double -> Coord -> Coord2 -> Double
calcField theta chi y w0 r_cyl gamma (xx,yy,_) (cx,cy)
        | r < r_cyl = 2*pi*chi*(1-y)*w0/gamma*((cos theta)**2-1/3)
        | otherwise = 2*pi*chi*(1-y)*w0/gamma*(sin theta)**2 * (r_cyl/r)**2 *
                      ((xx-cx)**2-(yy-cy)**2)/((xx-cx)**2+(yy-cy)**2)
        where r = dist2 (xx,yy) (cx,cy)

genCoordss2Field :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> [Coord] -> IO ([[Double]])
genCoordss2Field num_step num_cyl r_cyl0 l_cube l_step theta chi y w0 gamma [] = do return []
genCoordss2Field num_step num_cyl r_cyl0 l_cube l_step theta chi y w0 gamma (x:xs) = do
    coords <- randWalk num_step num_cyl r_cyl0 l_cube l_step x
    let fields = map (getField num_cyl theta chi y w0 r_cyl0 gamma l_cube) coords
    fieldss <- genCoordss2Field num_step num_cyl r_cyl0 l_cube l_step theta chi y w0 gamma xs
    return ([fields] ++ fieldss)

coordss2Fieldss :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> [[Coord]] -> Double -> [[Double]]
coordss2Fieldss num_cyl chi y w0 r_cyl0 gamma l_cube [] theta = []
coordss2Fieldss num_cyl chi y w0 r_cyl0 gamma l_cube (coords:coordss) theta = 
    [map (getField num_cyl theta chi y w0 r_cyl0 gamma l_cube) coords] ++ coordss2Fieldss num_cyl chi y w0 r_cyl0 gamma l_cube coordss theta

