-- BLOCH SOLVER MODULE
module BlochSolver
   (
     solveBloch
   ) where

import LinearAlgebra

solveBloch :: Double -> Double -> Double -> Double -> Double -> Vector -> Vector
solveBloch gamma t1 t2 t_step field m1 | a>0 = multMV (multDM (exp (-c2*t_step/3)) p1) m1
                                       | a<0 && g>1 = multMV (multDM (exp (-c2*t_step/3)) p2) m1
                                       | a<0 && g<1 = multMV (multDM (exp (-c2*t_step/3)) p3) m1
                                       | a<0 && g==1 = multMV (multDM (exp (-c2*t_step/3)) p4) m1
                                       | a==0 && b==0 = multMV (multDM (exp (-c2*t_step/3)) p5) m1
                                       where r1 = 0
                                             r2 = 0
                                             r3 = 0
                                             c0 = r3*(gamma*field)**2
                                             c1 = (gamma*field)**2 + r1*r2 + r1*r3 + r2*r3
                                             c2 = r1 + r2 + r3
                                             r_ = c2/3
                                             gamma_p = ((r1-r_, gamma*field, 0),
                                                        (-gamma*field, r2-r_, 0),
                                                        (0, 0, r3-r_))
                                             gamma_p2 = multMM gamma_p gamma_p
                                             a = c1 - 1/3*c2**2
                                             b = 2*(c2/3)**3 - c1*c2/3 + c0
                                             g = 0.5*abs b / (1/3*abs a)**1.5
                                             z1 = (-2)*sqrt (1/3*abs a)*sign b* sinh (1/3*asinh g)
                                             z2 = (-2)*sqrt (1/3*abs a)*sign b* cosh (1/3*acosh g)
                                             z3 = (-2)*sqrt (1/3*abs a)*sign b* cos (1/3*acos g)
                                             z4 = z2
                                             w1 = sqrt (abs a)*cosh (1/3*asinh g)
                                             w2 = sqrt (abs a)*sinh (1/3*acosh g)
                                             w3 = sqrt (abs a)*abs (sin (1/3*acos g))
                                             p1 = (1/(3*z1**2+a))<**>((exp (z1*t_step) <**> (((z1**2+a)<**>eye) <-> (z1<**>gamma_p) <+> gamma_p2)) <+>
                                                  ((exp (-z1*t_step/2)*cos (w1*t_step)) <**> (((2*z1**2)<**>eye)<+>(z1<**>gamma_p)<->gamma_p2))
                                                  <-> ((exp (-z1*t_step/2)*sin (w1*t_step)/w1) <**> (((a*z1)<**>eye) <+> ((a+3/2*z1**2)<**>gamma_p2))))
                                             p2 = (1/(3*z2**2+a))<**>((exp (z2*t_step) <**> (((z2**2+a)<**>eye) <-> (z2<**>gamma_p) <+> gamma_p2)) <+>
                                                  ((exp (-z2*t_step/2)*cos (w2*t_step)) <**> (((2*z2**2)<**>eye)<+>(z1<**>gamma_p)<->gamma_p2))
                                                  <-> ((exp (-z2*t_step/2)*sin (w2*t_step)/w2) <**> (((a*z2)<**>eye) <+> ((a+3/2*z2**2)<**>gamma_p2))))
                                             p3 = (1/(3*z3**2+a))<**>((exp (z3*t_step) <**> (((z3**2+a)<**>eye) <-> (z3<**>gamma_p) <+> gamma_p2)) <+>
                                                  ((exp (-z3*t_step/2)*cosh (w3*t_step)) <**> (((2*z3**2)<**>eye)<+>(z3<**>gamma_p)<->gamma_p2))
                                                  <-> ((exp (-z3*t_step/2)*sinh (w3*t_step)/w3) <**> (((a*z3)<**>eye) <+> ((a+3/2*z3**2)<**>gamma_p2))))
                                             p4 = ((1/9*exp (z1*t_step) + 8/9*exp (-z1*t_step/2) + z1/3*t_step*exp (-z1*t_step/2)) <**> eye) <+>
                                                  ((-4/9/z1*exp (-z1*t_step/2) - 1/3*t_step*exp (-z1*t_step/2))<**>gamma_p) <+> 
                                                  ((4/9/z1**2*exp (z1*t_step) - 4/9/z1**2*exp (-z1*t_step/2) - 2/3/z1*t_step*exp (-z1*t_step/2)) <**> gamma_p2)
                                             p5 = eye <-> (t_step<**>gamma_p) <+> ((1/2*t_step**2)<**>gamma_p2)