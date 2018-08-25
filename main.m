% CPMG MONTE CARLO SIMULATION

% CPMG CONSTANTS
NUM_POINT = 2000;                                              	% NUMBER OF MAGNETIZATION
NUM_ANGLE = 10;
T_ECHO = 8 * 10^(-3);                                        	% ECHO TIME FOR ACQUISITION (s)
T_STEP = 0.05 * 10^(-3);                                    	% TIME TAKEN FOR EACH STEP (s)
L_STEP = sqrt(6 * 10^(-9) * T_STEP);                       		% LENGTH OF EACH STEP (m)

% CYLINDER CONSTANTS
NUM_CYLINDER = 2;
GAMMA = 42.57748 * 10^6 * 2 * 3.1415926;                        % PROTON GYROMAGNETIC RATIO
CHI = 0.15 * 10^-6;                                      		% DIFFERENCE IN SUSCEPTIBILITY
Y = 0.70;                                                   	% DEGREE OF OXYGENATION OF BLOOD
W0 = 7 * GAMMA;                                               	% EXTERNAL FIELD FREQUENCY
R_CYLINDER = 9 * 10^(-6);                                     	% VESSEL RADIUS
BFR = 0.04;                                           			% RATIO OF VESSEL VOLUME VS CUBE VOLUME
L_CUBE = sqrt(3.1415926 * R_CYLINDER^2 / BFR);            		% EDGE OF THE CUBE AND THE LENGTH OF THE CYLINDER

Mplus_ = CPMG_T2(NUM_POINT, NUM_ANGLE, T_ECHO, T_STEP, L_STEP, L_CUBE, CHI, Y, W0, R_CYLINDER, NUM_CYLINDER, GAMMA, 0.07);

t = zeros(floor((13+1/2)*T_ECHO/T_STEP) + 1, 1);

for i = 0 : floor((13+1/2)*T_ECHO/T_STEP)   
    t(i+1) = i*T_STEP;
end

plot(t, Mplus_);