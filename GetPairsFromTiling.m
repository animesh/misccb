function pairs = GetPairsFromTiling(ContigRef, OverlapRef, NameHashRef, clipping_thrs, MinGapSize, MaxGapSize, AllowReverse, target_chromosome) 
%
% INPUT
% AllowReverse:    BOOLEAN True: reverse alignment on reference allowed


% Filter out reverse alignments to the reference
if ~AllowReverse
    OverlapRef = filter_reverse(OverlapRef);
end

% Create tiling and select the lines of the target chromosomes
unix(['show-tiling -v 80 out2.delta | sed -n ''/^>[\s]*' target_chromosome '/,/>/p'' | grep -v \> > tiling.txt']);
fid = fopen('tiling.txt');
tiling = textscan(fid,'%d%d%d%d%f%f%c%s','Delimiter','\t');
fclose(fid);

pairs = GeneratePairList(OverlapRef,NameHashRef, MinGapSize, MaxGapSize,tiling);


	function pairs = GeneratePairList(OverlapRef,NameHashRef, MinGapSize, MaxGapSize,tiling)

        % pairs                 [node1 node2 A1 A2 A3 A4]

        %                ---------------       -----------------
        %                          |||||       ||||||
        % ------------------------------------------------------------------------------
        %                          |   |       |    |
        %                          A3  A1      A2   A4

        pairs    = [];

        if isempty(tiling(1,1))
			return;
        end
		
		

		for t = 1:size(tiling{1},1)-1
			
			
            ov1 = GetOverlap(OverlapRef,tiling{8}{t});

            % Check if overlap is present in OverlapRef
            if length(ov1) ~= 1
                warning(['The overlap of ' tiling{8}{t} ' and the reference cannot be found' ...
                          'Possibly it is removed because it contained multiple hits or its ends are not well aligned']);
                continue
            end



            for t2 = t+1:size(tiling{1},1)
                Gap = tiling{1}(t2) - tiling{2}(t) - 1;
                if (Gap >= MinGapSize) && (Gap <= MaxGapSize) 

                    ov2 = GetOverlap(OverlapRef,tiling{8}{t2});

                    if length(ov2) ~= 1

                        if t2 == size(tiling{1},1)
                            warning(['The overlap of ' tiling{8}{t2} ' and the reference cannot be found' ...
                                      'This is a problem since it is defined as the endnode']);
                        else
                            warning(['The overlap of ' tiling{8}{t2} ' and the reference cannot be found' ...
                                      'Possibly it is removed because it contained multiple hits or its ends are not well aligned']);
                            continue
                        end
                        
                    else
                        pairs = [pairs; {ov1.Q ov2.Q ...
                                     ov1.E1 ov2.S1 ...
                                     ov1.S1 ov2.E1}];
                                 
                        break

                    end                    
                end
            end
        end
	end




	function OverlapRef = filter_reverse(OverlapRef)
        
        	%for overi = 1:size(Over
        	OverlapRef = OverlapRef(cell2mat({OverlapRef.E2})-cell2mat({OverlapRef.S2})>0);
        
	end

end
