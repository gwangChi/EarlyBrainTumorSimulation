module Main where

import RandomWalk
import LinearAlgebra
import BOLDfield

n = 4000
num_cyl = 1
gamma = 2*pi*42.57748e6
chi = 0.15e-6
y = 0.7
w0 = 7*gamma
r_cyl0 = 9e-6
bfr = 0.04
l_cube = sqrt(pi*r_cyl0**2/bfr)

main :: IO ()
main = do
       initCoords <- initCoord n l_cube
       let fields = fmap (getField num_cyl (pi/4) chi y w0 r_cyl0 gamma l_cube) initCoords
       coord <- randStep
       print coord
       --print (length3 coord)
       --print fields
