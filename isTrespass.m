% CHECK IF THE STEP HAS MADE THE PARTICLE TRAVEL OUTSIDE OR ENTER A CYLINDER

function flag = isTrespass(position_old, position_new, r_cylinder, l_cube, n_cylinder)

	if n_cylinder == 1
		flag = ((position_old(1)^2+position_old(2)^2)<r_cylinder^2 && (position_new(1)^2+position_new(2)^2)>r_cylinder^2) || ((position_old(1)^2+position_old(2)^2)>r_cylinder^2 && (position_new(1)^2+position_new(2)^2)<r_cylinder^2);
	elseif n_cylinder == 2

		r_cyl = r_cylinder/sqrt(2);

		c1 = l_cube * [-1/4; -1/4];
		c2 = l_cube * [1/4; 1/4];

		r1_old = sqrt((position_old(1) - c1(1))^2 + (position_old(2) - c1(2))^2);
		r2_old = sqrt((position_old(1) - c2(1))^2 + (position_old(2) - c2(2))^2);

		r1_new = sqrt((position_new(1) - c1(1))^2 + (position_new(2) - c1(2))^2);
		r2_new = sqrt((position_new(1) - c2(1))^2 + (position_new(2) - c2(2))^2);

		flag1 = (r1_old<r_cyl && r1_new>r_cyl) || (r1_old>r_cyl && r1_new<r_cyl);
		flag2 = (r2_old<r_cyl && r2_new>r_cyl) || (r2_old>r_cyl && r2_new<r_cyl);

		flag = flag1 || flag2;
	
	elseif n_cylinder == 4

		r_cyl = r_cylinder/2;

		c1 = l_cube * [-1/4; -1/4];
		c2 = l_cube * [1/4; -1/4];
		c3 = l_cube * [1/4; 1/4];
		c4 = l_cube * [-1/4; 1/4];

		r1_old = sqrt((position_old(1) - c1(1))^2 + (position_old(2) - c1(2))^2);
		r2_old = sqrt((position_old(1) - c2(1))^2 + (position_old(2) - c2(2))^2);
		r3_old = sqrt((position_old(1) - c3(1))^2 + (position_old(2) - c3(2))^2);
		r4_old = sqrt((position_old(1) - c4(1))^2 + (position_old(2) - c4(2))^2);

		r1_new = sqrt((position_new(1) - c1(1))^2 + (position_new(2) - c1(2))^2);
		r2_new = sqrt((position_new(1) - c2(1))^2 + (position_new(2) - c2(2))^2);
		r3_new = sqrt((position_new(1) - c3(1))^2 + (position_new(2) - c3(2))^2);
		r4_new = sqrt((position_new(1) - c4(1))^2 + (position_new(2) - c4(2))^2);

		flag1 = (r1_old<r_cyl && r1_new>r_cyl) || (r1_old>r_cyl && r1_new<r_cyl);
		flag2 = (r2_old<r_cyl && r2_new>r_cyl) || (r2_old>r_cyl && r2_new<r_cyl);
		flag3 = (r3_old<r_cyl && r3_new>r_cyl) || (r3_old>r_cyl && r3_new<r_cyl);
		flag4 = (r4_old<r_cyl && r4_new>r_cyl) || (r4_old>r_cyl && r4_new<r_cyl);

		flag = flag1 || flag2 || flag3 || flag4;
		
	elseif n_cylinder == 9

		r_cyl = r_cylinder/3;

		c1 = l_cube * [-1/3; -1/3];
		c2 = l_cube * [0; -1/3];
		c3 = l_cube * [1/3; -1/3];
		c4 = l_cube * [1/3; 0];
		c5 = l_cube * [1/3; 1/3];
		c6 = l_cube * [0; 1/3];
		c7 = l_cube * [-1/3; 1/3];
		c8 = l_cube * [-1/3; 0];
		c9 = l_cube * [0; 0];

		r1_old = sqrt((position_old(1) - c1(1))^2 + (position_old(2) - c1(2))^2);
		r2_old = sqrt((position_old(1) - c2(1))^2 + (position_old(2) - c2(2))^2);
		r3_old = sqrt((position_old(1) - c3(1))^2 + (position_old(2) - c3(2))^2);
		r4_old = sqrt((position_old(1) - c4(1))^2 + (position_old(2) - c4(2))^2);
		r5_old = sqrt((position_old(1) - c5(1))^2 + (position_old(2) - c5(2))^2);
		r6_old = sqrt((position_old(1) - c6(1))^2 + (position_old(2) - c6(2))^2);
		r7_old = sqrt((position_old(1) - c7(1))^2 + (position_old(2) - c7(2))^2);
		r8_old = sqrt((position_old(1) - c8(1))^2 + (position_old(2) - c8(2))^2);
		r9_old = sqrt((position_old(1) - c9(1))^2 + (position_old(2) - c9(2))^2);

		r1_new = sqrt((position_new(1) - c1(1))^2 + (position_new(2) - c1(2))^2);
		r2_new = sqrt((position_new(1) - c2(1))^2 + (position_new(2) - c2(2))^2);
		r3_new = sqrt((position_new(1) - c3(1))^2 + (position_new(2) - c3(2))^2);
		r4_new = sqrt((position_new(1) - c4(1))^2 + (position_new(2) - c4(2))^2);
		r5_new = sqrt((position_new(1) - c5(1))^2 + (position_new(2) - c5(2))^2);
		r6_new = sqrt((position_new(1) - c6(1))^2 + (position_new(2) - c6(2))^2);
		r7_new = sqrt((position_new(1) - c7(1))^2 + (position_new(2) - c7(2))^2);
		r8_new = sqrt((position_new(1) - c8(1))^2 + (position_new(2) - c8(2))^2);
		r9_new = sqrt((position_new(1) - c9(1))^2 + (position_new(2) - c9(2))^2);

		flag1 = (r1_old<r_cyl && r1_new>r_cyl) || (r1_old>r_cyl && r1_new<r_cyl);
		flag2 = (r2_old<r_cyl && r2_new>r_cyl) || (r2_old>r_cyl && r2_new<r_cyl);
		flag3 = (r3_old<r_cyl && r3_new>r_cyl) || (r3_old>r_cyl && r3_new<r_cyl);
		flag4 = (r4_old<r_cyl && r4_new>r_cyl) || (r4_old>r_cyl && r4_new<r_cyl);
		flag5 = (r5_old<r_cyl && r5_new>r_cyl) || (r5_old>r_cyl && r5_new<r_cyl);
		flag6 = (r6_old<r_cyl && r6_new>r_cyl) || (r6_old>r_cyl && r6_new<r_cyl);
		flag7 = (r7_old<r_cyl && r7_new>r_cyl) || (r7_old>r_cyl && r7_new<r_cyl);
		flag8 = (r8_old<r_cyl && r8_new>r_cyl) || (r8_old>r_cyl && r8_new<r_cyl);
		flag9 = (r9_old<r_cyl && r9_new>r_cyl) || (r9_old>r_cyl && r9_new<r_cyl);

		flag = flag1 || flag2 || flag3 || flag4 || flag5 || flag6 || flag7 || flag8 || flag9;
	
	else
		disp('This number of cylinders cannot be processed');
	end 

end