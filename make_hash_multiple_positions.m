function h = make_hash_multiple_positions(names)    
    unames = unique(names);

    [snames I] = sort(names);

    pos = 1; n = '-1'; loc = [];

    hi  = cell(length(unames),1);
    fprintf('Generating overlap hash tables\n');
    fprintf('10000 names of %d processed per dot [',length(snames));    for i = 1:length(snames)

        if mod(i,1000) == 0
            fprintf('.');
            if mod(i,50000) == 0
                fprintf('\n');
            end
        end

        if i == 1
		n   = snames{i};
                loc = I(i);
                pos = 1;

        
	
	else
		% Get name
		ni = snames{i};

		% Check if name is same as previous iteration
		if isequal(ni,n)
			loc = [loc I(i)];
		else
		        % Store locations for this name
			hi{pos}      = loc;
			unames{pos}  = n;

		        % Initialize for new name
			pos  	     = pos + 1;
			loc 	     = I(i);
			n	     = ni;		
	  	end
	end  

	% Store last value
	if i == length(snames)
	        % Store locations for this name
		hi{pos}      = loc;
		unames{pos}  = n;
	end


    end; % cell array with all positions
    fprintf('] done \n');    h = containers.Map(unames, hi);end

