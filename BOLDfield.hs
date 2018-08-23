-- BOLD FIELD MODULE
module BOLDfield
   (
      getField
   ) where

import LinearAlgebra

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