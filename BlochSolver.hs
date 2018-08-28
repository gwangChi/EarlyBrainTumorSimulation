-- BLOCH SOLVER MODULE
module BlochSolver
   (
     solveBloch
   , evolveMags
   ) where

import LinearAlgebra
import Control.Parallel (par, pseq)

-- GIVEN A MAG, SOLVE FOR THE NEW MAG BASED ON TIME INTERVAL AND CURRENT B FIELD
solveBloch :: Double -> Double -> Double -> Vector -> Vector -> Vector
solveBloch t1 t2 t_step (bx,by,bz) m1 | a>0 = multMV (exp (-c2*t_step/3) <**> p1) m1
                                            | a<0 && gamm>1 = multMV (exp (-c2*t_step/3) <**> p2) m1
                                            | a<0 && gamm<1 = multMV (exp (-c2*t_step/3) <**> p3) m1
                                            | a<0 && gamm==1 = multMV (exp (-c2*t_step/3) <**> p4) m1
                                            | a==0 && b==0 = multMV (exp (-c2*t_step/3) <**> p5) m1
                                            where
                                                  gamma = 2*pi*42.57748e6
                                                  r1 | t2 == 0 = 0
                                                     | otherwise = 1/t2
                                                  r2 | t2 == 0 = 0
                                                     | otherwise = 1/t2
                                                  r3 | t1 == 0 = 0
                                                     | otherwise = 1/t1
                                                  w1 = (-1)*gamma*bx
                                                  w2 = (-1)*gamma*by
                                                  w3 = (-1)*gamma*bz
                                                  c0 = r1*(w1**2) + r2*(w2**2) + r3*(w3**2)
                                                  c1 = w1**2 + w2**2 + w3**2 + r1*r2 + r2*r3 + r1*r3
                                                  c2 = r1 + r2 + r3
                                                  r_ = c2/3
                                                  gamma_p = ((r1-r_,-w3,w2),(w3,r2-r_,-w1),(-w2,w1,r3-r_))
                                                  a = c1 - 1/3*(c2**2)
                                                  b = 2/27*(c2**3) - 1/3*c1*c2 + c0
                                                  alph = 1/3*(abs a)
                                                  beta = 1/2*(abs b)
                                                  gamm = beta/(alph**(1.5))

                                                  z1 = -2*(sqrt alph)*(sign b)*(sinh (1/3*(asinh gamm)))
                                                  z2 = -2*(sqrt alph)*(sign b)*(cosh (1/3*(acosh gamm)))
                                                  z3 = -2*(sqrt alph)*(sign b)*(cos (1/3*(acos gamm)))
                                                  z4 = z2
                                                
                                                  w_1 = sqrt (3*alph)*(cosh (1/3*(asinh gamm)))
                                                  w_2 = sqrt (3*alph)*(sinh (1/3*(acosh gamm)))
                                                  w_3 = abs ((sqrt (3*alph))*(sin (1/3*(acos gamm))))
                                             
                                                  gamma_p2 = multMM gamma_p gamma_p
                                               
                                                  p1 = ((exp (-z1*t_step/2))/(a + 3*(z1**2))) <**> ((((a + z1**2)*exp (3/2*z1*t_step) + 2*(z1**2)*cos (w_1*t_step) - a/w_1*z1*sin (w_1*t_step)) <**> eye) <+> 
                                                                                                   (((-z1)*exp (3/2*z1*t_step) + z1*cos (w_1*t_step) - (a + 3/2*(z1**2))/w_1*sin (w_1*t_step)) <**> gamma_p) <+> 
                                                                                                   ((exp (3/2*z1*t_step) - cos (w_1*t_step) - 3/2/w_1*z1*sin (w_1*t_step)) <**> gamma_p2))                                            
                                             
                                                  p2 = ((exp (-z1*t_step/2))/(a + 3*(z1**2))) <**> ((((a + z2**2)*exp (3/2*z2*t_step) + 2*(z2**2)*cos (w_2*t_step) - a/w_2*z2*sin (w_2*t_step)) <**> eye) <+> 
                                                                                                   (((-z2)*exp (3/2*z2*t_step) + z1*cos (w_2*t_step) - (a + 3/2*(z2**2))/w_2*sin (w_2*t_step)) <**> gamma_p) <+> 
                                                                                                   ((exp (3/2*z2*t_step) - cos (w_2*t_step) - 3/2/w_2*z2*sin (w_2*t_step)) <**> gamma_p2))   

                                                  p3 = ((exp (-z1*t_step/2))/(a + 3*(z1**2))) <**> ((((a + z3**2)*exp (3/2*z3*t_step) + 2*(z3**2)*cosh (w_3*t_step) - a/w_3*z3*sinh (w_3*t_step)) <**> eye) <+> 
                                                                                                   (((-z3)*exp (3/2*z3*t_step) + z3*cosh (w3*t_step) - (a + 3/2*(z3**2))/w_3*sinh (w_3*t_step)) <**> gamma_p) <+> 
                                                                                                   ((exp (3/2*z3*t_step) - cosh (w_3*t_step) - 3/2/w_3*z3*sinh (w_3*t_step)) <**> gamma_p2))
   
                                                  p4 = ((exp (-z1*t_step/2))) <**> (((1/9*exp (3/2*z4*t_step) + 8/9 + t_step/3*z1) <**> eye) <+>
                                                                                   ((-4/9/z4*exp (3/2*z4*t_step) + 4/9/z4 - t_step/3) <**> gamma_p) <+>
                                                                                   ((4/9/(z4**2)*exp (3/2*z4*t_step) - 4/9/(z4**2) - 2*t_step/3/z4) <**> gamma_p2))

                                                  p5 = eye <+> ((-t_step) <**> gamma_p) <+> (((1/2*(t_step**2)) <**> gamma_p2))



-- EVOLVES 1 MAGNETIZATION VECTOR FOR THE WHOLE COURSE
evolveMag :: Double -> [Double] -> Double -> Double -> Double -> [Vector] -> Vector -> [Vector]
evolveMag currStep flips t1 t2 t_step [] _ = []
evolveMag currStep flips t1 t2 t_step (field:fields) (mx,my,mz) | elem (currStep*t_step) flips = [m2_] ++ evolveMag (currStep+1) flips t1 t2 t_step fields m2_
                                                                | otherwise = [m2] ++ evolveMag (currStep+1) flips t1 t2 t_step fields m2
                                                                where m2 = solveBloch t1 t2 t_step field (mx,my,mz)
                                                                      m2_ = solveBloch t1 t2 t_step field (-mx,my,-mz)
                                                                                      

-- EVOLVES N MAGNETIZATIONS FOR THE WHOLE COURSE
evolveMags :: [Double] -> Double -> Double -> Double -> [Vector] -> [[Vector]] -> [[Vector]]
evolveMags flips t1 t2 t_step [] _ = []
evolveMags flips t1 t2 t_step (m:ms) (fields:fieldss) = [evolveMag 0 flips t1 t2 t_step fields m] ++ evolveMags flips t1 t2 t_step ms fieldss










