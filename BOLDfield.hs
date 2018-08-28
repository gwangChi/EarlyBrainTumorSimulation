-- BOLD FIELD MODULE
module BOLDfield
   (
      getField
    , genCoordss2Field
    , coordss2Fieldss
    , genFieldsss
   ) where

import LinearAlgebra
import RandomWalk
import Control.Exception.Base (evaluate)
import Control.DeepSeq (force)

getField :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Coord -> Vector
getField num_cyl theta chi y w0 r_cyl0 l_cube coord
       = foldl (plusV3) (0,0,0) (map (calcField theta chi y w0 (r_cyl0/(sqrt num_cyl)) coord) (map (mult2 l_cube) (choose num_cyl)))
   where choose :: Double -> [Coord2]
         choose n | n==1 = [(0,0)]
                  | n==2 = [(-1/4,-1/4),(1/4,1/4)]
                  | n==4 = [(-1/4,-1/4),(1/4,-1/4),(1/4,1/4),(-1/4,1/4)]
                  | n==9 = [(-1/3,-1/3),(0,-1/3),(1/3,-1/3),(1/3,0),(1/3,1/3),(0,1/3),(-1/3,1/3),(-1/3,0),(0,0)]

calcField :: Double -> Double -> Double -> Double -> Double -> Coord -> Coord2 -> Vector
calcField theta chi y w0 r_cyl (xx,yy,_) (cx,cy)
        | r < r_cyl = (0,0,2*pi*chi*(1-y)*w0/gamma*((cos theta)**2-1/3))
        | otherwise = (0,0,2*pi*chi*(1-y)*w0/gamma*(sin theta)**2 * (r_cyl/r)**2 *
                      ((xx-cx)**2-(yy-cy)**2)/((xx-cx)**2+(yy-cy)**2))
        where r = dist2 (xx,yy) (cx,cy)
              gamma = 2*pi*42.57748e6

genCoordss2Field :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> [Coord] -> IO ([[Vector]])
genCoordss2Field num_step num_cyl r_cyl0 l_cube l_step theta chi y w0 [] = do return []
genCoordss2Field num_step num_cyl r_cyl0 l_cube l_step theta chi y w0 (x:xs) = do
    coords <- randWalk num_step num_cyl r_cyl0 l_cube l_step x
    let fields = map (getField num_cyl theta chi y w0 r_cyl0 l_cube) coords
    fieldss <- genCoordss2Field num_step num_cyl r_cyl0 l_cube l_step theta chi y w0 xs
    return ([fields] ++ fieldss)

coordss2Fieldss :: Double -> Double -> Double -> Double -> Double -> Double -> [[Coord]] -> Double -> [[Vector]]
coordss2Fieldss num_cyl chi y w0 r_cyl0 l_cube [] theta = []
coordss2Fieldss num_cyl chi y w0 r_cyl0 l_cube (coords:coordss) theta = 
    [map (getField num_cyl theta chi y w0 r_cyl0 l_cube) coords] ++ coordss2Fieldss num_cyl chi y w0 r_cyl0 l_cube coordss theta

genFieldsss :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> [Double] -> IO ([[[Vector]]])
genFieldsss n num_step l_cube num_cyl r_cyl0 l_step chi y w0 thetas = do
    initCoords <- initCoord n l_cube
    coordss <- walkAll num_step num_cyl r_cyl0 l_cube l_step initCoords
    --evaluate $ force (parMap rpar (coordss2Fieldss num_cyl chi y w0 r_cyl0 l_cube coordss) thetas)
    evaluate $ force (map (coordss2Fieldss num_cyl chi y w0 r_cyl0 l_cube coordss) thetas)