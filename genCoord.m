% GENERATE A NEW COORDINATE IF THE GENERATED POSITION IS OUT OF THE CUBE

function new_position = genCoord(position, L_CUBE)

    new_position = position;

    for i = 1:3
        
        if new_position(i) > L_CUBE/2
            new_position(i) = new_position(i) - L_CUBE;
        end
        if new_position(i) < -L_CUBE/2
            new_position(i) = new_position(i) + L_CUBE;
        end
        
    end

end






