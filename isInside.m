% Check if coordinate is inside cube
function flag = isInside(position, l_cube)
    flag = (abs(position(1))<=l_cube/2)&&(abs(position(2))<=l_cube/2)&&(abs(position(3))<=l_cube/2);
end
