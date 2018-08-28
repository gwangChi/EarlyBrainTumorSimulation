module Main
where
import System.IO
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies(parMap, rpar)
import Control.DeepSeq (force)
import Control.Exception.Base (evaluate)
import RandomWalk
import LinearAlgebra
import BOLDfield
import BlochSolver

np = 4000
nstep = 2000
ncyl = 1

flips = []

theta = pi/4
thetas = map ((pi/17)*) [1..16]

t1=0
t2=0

gamma = 2*pi*42.57748e6
chi = 1.5e-7
y = 0.7
w0 = 7*gamma
rcyl0 = 9e-6
bfr = 0.04
lcube = sqrt(pi*r_cyl0**2/bfr)
tstep = 5e-5
lstep = sqrt (t_step*6e-9)

main :: IO ()
main = do
       fieldsss <- genFieldsss np nstep lcube ncyl rcyl0 lstep chi y w0 thetas
       magsss <- evaluate $ force (map (evolveMags flips t1 t2 tstep (duplicateVector n (0,1,0))) fieldsss)

       let result = map getTransNorm (avgThruThetas thetas (map avgMagss2Mags magsss))

       outh <- openFile "mag.txt" WriteMode
       hPutStrLn outh (showPrettyDoubles result)
       hClose outh
       putStrLn "Finished"
       return ()









{--    Test Utilities
       initCoords <- initCoord n l_cube
       coordss <- walkAll num_step num_cyl r_cyl0 l_cube l_step initCoords

       --let fieldsss = parMap rpar (coordss2Fieldss num_cyl chi y w0 r_cyl0 gamma l_cube coordss) thetas
       fieldsss <- evaluate (force (map (coordss2Fieldss num_cyl chi y w0 r_cyl0 gamma l_cube coordss) thetas))

       initMags <- evaluate (force (duplicateVector n (0,1,0)))
       --let magsss = parMapEvolveMags (evolveMags flips gamma t1 t2 t_step initMags) (map doubless2Vectorss fieldsss)
       magsss <- evaluate (force (map (evolveMags flips gamma t1 t2 t_step initMags) (map doubless2Vectorss fieldsss)))
       putStrLn ("Magsss has "++(show (length magsss))++" angles")

       avgmagss <- evaluate (force (parMapAvgMagss avgMagss2Mags magsss))
       --let avgmagss = map avgMagss2Mags magsss
       putStrLn ("AvgMagss has "++(show (length avgmagss))++" angles")

       --let result = map getTransNorm (avgThruThetas thetas avgmagss)
       
       --outh <- openFile "mag.txt" WriteMode
       outh <- openFile "avgmagss.txt" WriteMode
       hPutStrLn outh (showPrettyVectorss avgmagss)
       hClose outh

--}



