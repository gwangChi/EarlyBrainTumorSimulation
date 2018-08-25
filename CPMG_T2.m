% SPINECHO SEQUENCE
% WALK AND THEN EVOLVE ACCORDINGLY
% FLIP AT HALF T_ECHO

function Mplus_ = CPMG_T2(NUM_POINT, NUM_ANGLE, T_ECHO, T_STEP, L_STEP, L_CUBE, CHI, Y, W0, R_CYLINDER, NUM_CYLINDER, GAMMA, T2)
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%UNIVERSAL CONSTANTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    PI = 3.1415926;
    
    SUM_SINE = 0;

    for theta = 1 : NUM_ANGLE
    	SUM_SINE = SUM_SINE + sin(theta*PI / (NUM_ANGLE+1));
    end

    % NORMALIZATION CONSTANT
    NORM = NUM_POINT * SUM_SINE;
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%INITIALIZATION%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % GENERATE N_P RANDOM POSITIONS
    rng(10);
    position = zeros(3, NUM_POINT, floor((13+1/2)*T_ECHO/T_STEP) + 1);
    position(:, :, 1) = rand([3 NUM_POINT])*L_CUBE - [ones([1, NUM_POINT]); ones([1, NUM_POINT]); ones([1, NUM_POINT])]*L_CUBE/2;  

    % STORAGE OF MAGNETIZATION VECTOR AT EACH POSITION AND ANGLE AT AN INSTANT
    M = zeros(3, NUM_POINT, NUM_ANGLE);
    M_1 = zeros(3, floor((13+1/2)*T_ECHO/T_STEP) + 1);

    % STORAGE OF MPLUS AT EVERY STEP
    Mplus_ = zeros(floor((13+1/2)*T_ECHO/T_STEP) + 1, 1);

    % AFTER THE INITIAL 90 DEGREE PULSE, ALL OF THE MAGNETIZATION IS IN THE Y-DIRECTION EXT B FIELD COORDINATE
    for theta = 1 : NUM_ANGLE
    	M(:, :, theta) = [0 * ones([1, NUM_POINT]); ones([1, NUM_POINT]); 0 * ones([1, NUM_POINT])];
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%WALK%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % ONE STEP FORWARD FOR EACH POSITIONS
    
    position_1 = zeros(3, floor((13+1/2)*T_ECHO/T_STEP) + 1);
    position_1(:, 1) = position(:, 1, 1);
    
    for curr_step = 1 : floor((13+1/2)*T_ECHO/T_STEP) + 1
        
        curr_point = 1;
        
        while curr_point <= NUM_POINT

            % GENERATE A RANDOM STEP VECTOR POINTING TO SURFACE OF UNIT SPHERE
            u = -1 + rand*2;
            phi = rand*2*PI;                  
            step = L_STEP*[sqrt(1-u^2)*cos(phi); sqrt(1-u^2)*sin(phi); u];
            position(:, curr_point, curr_step+1) = position(:, curr_point, curr_step) + step;

            % CALIBRATE THE NEW POSITION
            while isInside(position(:, curr_point, curr_step+1), L_CUBE) == false
                position(:, curr_point, curr_step+1) = genCoord(position(:, curr_point, curr_step+1), L_CUBE);
            end

            if isTrespass(position(:, curr_point, curr_step), position(:, curr_point, curr_step+1), R_CYLINDER, L_CUBE, NUM_CYLINDER)
                curr_point = curr_point - 1;
            end
            
            curr_point = curr_point + 1;

        end
        
        position_1(:, curr_step + 1) = position(:, 1, curr_step + 1);
        
    end
    
    plot3(position_1(1, :)', position_1(2, :)', position_1(3, :)');
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%COLLECT AND EVOLVE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for curr_step = 1 : floor((13+1/2)*T_ECHO/T_STEP) + 1

        % FLIP MAGNETIZATION AT T_ECHO/2 AND (N+1/2)T_ECHO BY THE PI_Y PULSE
        if curr_step == floor(T_ECHO/2/T_STEP) + 1 || (curr_step > floor(T_ECHO/2/T_STEP) + 1 && mod(curr_step - floor(T_ECHO/2/T_STEP) - 1, floor(T_ECHO/T_STEP)) == 0)

        	for curr_theta = 1 : NUM_ANGLE

            	for curr_point = 1 : NUM_POINT

                	M(:, curr_point, curr_theta) = [-1 0 0; 0 1 0; 0 0 -1] * M(:, curr_point, curr_theta);
  
            	end

        	end

        end

        % CALCULATE THE AVERAGE M+
        M_ = zeros(3, 1);

        for curr_theta = 1 : NUM_ANGLE

            for curr_point = 1 : NUM_POINT

                M_ = M_ + sin(curr_theta*PI / (NUM_ANGLE+1)) / NORM * M(:, curr_point, curr_theta);
  
            end

        end

        Mplus_(curr_step) = sqrt(abs(M_(1))^2 +abs(M_(2))^2);
        M_1(:, curr_step) = M(:, 1, 1);

        % EVOLVE THE MAGENETIZATION ACCORDING TO THE ANALYTICAL SOLUTION
        for curr_theta = 1 : NUM_ANGLE

            for curr_point = 1 : NUM_POINT

                curr_B =  getField(position(:, curr_point, curr_step), curr_theta*PI / (NUM_ANGLE+1), CHI, Y, W0, R_CYLINDER, GAMMA, L_CUBE, NUM_CYLINDER);

                % DEFINITION OF CONSTANTS AND MATRICES GOES HERE
                R1 = 1/T2;
                R2 = 1/T2;
                R3 = 0;
                
                W1 = (-1)*GAMMA*curr_B(1); % omega1 = -gamma*Bx
                W2 = (-1)*GAMMA*curr_B(2); % omega1 = -gamma*By
                W3 = (-1)*GAMMA*curr_B(3); % omega1 = -gamma*Bz
         
                C0 = R1*W1^2 + R2*W2^2+ R3*W3^2;
                C1 = (W1^2 + W2^2 + W3^2 + R1*R2 + R2*R3 +R1*R3);
                C2 = R1 + R2 + R3;
                
                R_ = C2/3;

                GAMMA_P = [R1-R_ W3 -W2; -W3 R2-R_ W1; W2 -W1 R3-R_];
                
                a = C1 - C2^2/3;
                b = 2*(C2/3)^3 - C1*C2/3 + C0;
                
                alpha = abs(a)/3;
                beta = abs(b)/2;
                gam = beta/(alpha^(3/2));
                
                if a > 0
                    Z1 = -2*sqrt(alpha)*sign(b)*sinh(1/3*asinh(gam));
                    W_ = sqrt(3*alpha)*cosh(1/3*asinh(gam));
                    PROPAGATOR_P = 1/(3*Z1^2 + a) * (exp(Z1*T_STEP)*((Z1^2 + a)*eye(3) - Z1*GAMMA_P + GAMMA_P^2) + exp(-Z1*T_STEP/2)*(2*Z1^2*eye(3) + Z1*GAMMA_P - GAMMA_P^2)*cos(W_*T_STEP) - exp(-Z1*T_STEP/2)*(a*Z1*eye(3) + (3/2*Z1^2 + a)*GAMMA_P +3/2*Z1*GAMMA_P^2)*sin(W_*T_STEP)/W_);
                elseif a < 0
                    if gam > 1
                        Z1 = -2*sqrt(alpha)*sign(b)*cosh(1/3*acosh(gam));
                        W_ = sqrt(3*alpha)*sinh(1/3*acosh(gam));
                        PROPAGATOR_P = 1/(3*Z1^2 + a) * (exp(Z1*T_STEP)*((Z1^2 + a)*eye(3) - Z1*GAMMA_P + GAMMA_P^2) + exp(-Z1*T_STEP/2)*(2*Z1^2*eye(3) + Z1*GAMMA_P - GAMMA_P^2)*cos(W_*T_STEP) - exp(-Z1*T_STEP/2)*(a*Z1*eye(3) + (3/2*Z1^2 + a)*GAMMA_P +3/2*Z1*GAMMA_P^2)*sin(W_*T_STEP)/W_);
                    elseif gam < 1
                        Z1 = -2*sqrt(alpha)*sign(b)*cos(1/3*acos(gam));
                        W_ = 1i * sqrt(3*alpha)*sin(1/3*acos(gam));
                        PROPAGATOR_P = 1/(3*Z1^2 + a) * (exp(Z1*T_STEP)*((Z1^2 + a)*eye(3) - Z1*GAMMA_P + GAMMA_P^2) + exp(-Z1*T_STEP/2)*(2*Z1^2*eye(3) + Z1*GAMMA_P - GAMMA_P^2)*cosh(abs(W_)*T_STEP) - exp(-Z1*T_STEP/2)*(a*Z1*eye(3) + (3/2*Z1^2 + a)*GAMMA_P +3/2*Z1*GAMMA_P^2)*sinh(abs(W_)*T_STEP)/abs(W_));
                    elseif gam == 1
                        Z1 = -2*sqrt(alpha)*sign(b)*cosh(1/3*acosh(gam));
                        PROPAGATOR_P = (1/9*exp(Z1*T_STEP) + 8/9*exp(-Z1*T_STEP/2) + Z1/3*T_STEP*exp(-Z1*T_STEP/2))*eye(3) + (-4/9/Z1*exp(Z1*T_STEP) + 4/9/Z1*exp(-Z1*T_STEP/2) - 1/3*T_STEP*exp(-Z1*T_STEP/2))*GAMMA_P + (4/9/Z1^2*exp(Z1*T_STEP) - 4/9/Z1^2*exp(-Z1*T_STEP/2) - 2/3/Z1*T_STEP*exp(-Z1*T_STEP/2))*GAMMA_P^2;
                    end
                elseif a == 0 && b == 0
                    PROPAGATOR_P = eye(3) - GAMMA_P*T_STEP + 1/2*GAMMA_P^2*T_STEP^2;
                end
                
                % USING PROPAGATOR MATRIX TO PROPAGATE ONE STEP
                M(:, curr_point, curr_theta) = exp(-C2*T_STEP/3)*PROPAGATOR_P*M(:, curr_point, curr_theta);

                %M(:, curr_point, curr_theta) = M(:, curr_point, curr_theta) / sqrt(dot(M(:, curr_point, curr_theta), M(:, curr_point, curr_theta)));
            end

        end

    end

    % NORMALIZATION OF M+ WITH REGARD TO THE INITIAL VALUE
    Mplus_ = Mplus_ / Mplus_(1);
    plot3(M_1(1, :)', M_1(2, :)', M_1(3, :)');
    
end