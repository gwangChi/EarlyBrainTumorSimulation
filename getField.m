function B = getField(position, theta, CHI, Y, W0, R_CYLINDER, GAMMA, L_CUBE, NUM_CYLINDER)

	C = zeros(2, NUM_CYLINDER);

	% ASSIGN THE POSITION OF THE CENTER OF THE CYLINDERS BASED ON THE NUMBER OF CYLINDERS
	if NUM_CYLINDER == 1

		r_cyl = R_CYLINDER;

		C(:, 1) = [0; 0];

	elseif NUM_CYLINDER == 2

		r_cyl = R_CYLINDER/sqrt(2);

		C(:, 1) = L_CUBE * [-1/4; -1/4];
		C(:, 2) = L_CUBE * [1/4; 1/4];

	elseif NUM_CYLINDER == 4

		r_cyl = R_CYLINDER/2;

		C(:, 1) = L_CUBE * [-1/4; -1/4];
		C(:, 2) = L_CUBE * [1/4; -1/4];
		C(:, 3) = L_CUBE * [1/4; 1/4];
		C(:, 4) = L_CUBE * [-1/4; 1/4];

	elseif NUM_CYLINDER == 9
	
		r_cyl = R_CYLINDER/3;

		C(:, 1) = L_CUBE * [-1/3; -1/3];
		C(:, 2) = L_CUBE * [0; -1/3];
		C(:, 3) = L_CUBE * [1/3; -1/3];
		C(:, 4) = L_CUBE * [1/3; 0];
		C(:, 5) = L_CUBE * [1/3; 1/3];
		C(:, 6) = L_CUBE * [0; 1/3];
		C(:, 7) = L_CUBE * [-1/3; 1/3];
		C(:, 8) = L_CUBE * [-1/3; 0];
		C(:, 9) = L_CUBE * [0; 0];

	else
		disp('This number of cylinders cannot be processed.');
	end

	B = zeros(3, 1);

	for i = 1 : NUM_CYLINDER

		% GET THE DISTANCE BETWEEN PARTICLE AND THE CENTER OF THE ITH CYLINDER
		r = sqrt((position(1) - C(1, i))^2 + (position(2) - C(2, i))^2);

		% OBTAIN THE MAGNETIC FIELD FROM THE INTERACTION WITH THE ITH CYLINDER IN THE ROTATING FRAME
        if r < r_cyl
			B = B + [0; 0; 2*3.1415926*CHI*(1-Y)*W0/GAMMA*((cos(theta))^2-1/3)];
    	else
        	B = B + [0; 0; 2*3.1415926*CHI*(1-Y)*W0/GAMMA*(sin(theta))^2*(r_cyl/r)^2*((position(1) - C(1, i))^2-(position(2) - C(2, i))^2)/((position(1) - C(1, i))^2+(position(2) - C(2, i))^2)];       
    	end

    end
    
end