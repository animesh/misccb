function [pairs, deltafiles_ref, OverlapRef, ContigRef] = GetPairsFromFuzzyTiling(ContigRef, OverlapRef, NameHashRef, RefFasta, clipping_thrs, MinGapSize, MaxGapSize, deltafiles_ref,DeltaRefUpdated) 
%
% INPUT
% AllowReverse:    BOOLEAN True: reverse alignment on reference allowed


% Filter out reverse alignments to the reference
%if ~AllowReverse
%    OverlapRef = filter_reverse(OverlapRef);
%end


% Set the number of steps still allowed to make a connection between 2
% contigs in the double for loop
% At least one link should be found though to break the loop
max_tiling_steps = 2;
found_link       = false;


% Create tiling
!show-tiling  -i 60 -v 80 out2.delta | grep -v "^>" > tiling.txt;
fid = fopen('tiling.txt');
tiling = textscan(fid,'%d%d%d%d%f%f%c%s','Delimiter','\t');
fclose(fid);

[pairs, deltafiles_ref, OverlapRef] = GeneratePairList(OverlapRef,NameHashRef, MinGapSize, MaxGapSize,tiling,deltafiles_ref);


	function [pairs, deltafiles_ref, OverlapRef] = GeneratePairList(OverlapRef,NameHashRef, MinGapSize, MaxGapSize, tiling, deltafiles_ref)

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
		
        
		

		for t1 = 1:size(tiling{1},1)-1
            found_link = false;
            for t2 = t1+1:size(tiling{1},1)
                
                if (t2-t1 > max_tiling_steps) && found_link
                    break
                end
                
                Gap = tiling{1}(t2)-tiling{2}(t1)-1;
                
                % Allow certain gap sizes
                if (Gap >= MinGapSize) && (Gap <= MaxGapSize) && (Gap >= 0)
			
                    found_link = true;
                    
                    pairs = [pairs; tiling{8}(t1) tiling{8}(t2) ...
                                        tiling{2}(t1) tiling{1}(t2) tiling{1}(t1) tiling{2}(t2)];
                                    
                    % Update Deltafile
                    
                    if ~isKey(DeltaRefUpdated,tiling{8}(t1))
                        deltafiles_ref = UpdateDeltafile(deltafiles_ref,tiling{8}(t1),tiling{1}(t1), tiling{2}(t1), tiling{7}(t1));
                        OverlapRef     = UpdateOverlapRef(OverlapRef,ContigRef,tiling{8}(t1),tiling{1}(t1), tiling{2}(t1), tiling{7}(t1));
                        DeltaRefUpdated(tiling{8}{t1}) = 1;
                    end
                    if ~isKey(DeltaRefUpdated,tiling{8}(t2)) 
                        deltafiles_ref = UpdateDeltafile(deltafiles_ref,tiling{8}(t2),tiling{1}(t2), tiling{2}(t2), tiling{7}(t2));
                        OverlapRef     = UpdateOverlapRef(OverlapRef,ContigRef,tiling{8}(t2),tiling{1}(t2), tiling{2}(t2), tiling{7}(t2));
                        DeltaRefUpdated(tiling{8}{t1}) = 1;
                    end
                    
                    % Update OverlapRef
                    
                    
                    
                end
            end
		end

	end



	function pairs = GeneratePairList_old(OverlapRef,NameHashRef, MinGapSize, MaxGapSize,tiling)

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
			
			if (tiling{3}(t) >= MinGapSize) && (tiling{3}(t) <= MaxGapSize) 
			
			    ov1 = GetOverlap(OverlapRef,tiling{8}{t});
		            ov2 = GetOverlap(OverlapRef,tiling{8}{t+1});
			
			    pairs = [pairs; {ov1.Q ov2.Q ...
                       	         	     ov1.E1 ov2.S1 ...
                        	             ov1.S1 ov2.E1}];
			end
		end

    end



    function deltafiles_ref = UpdateDeltafile(deltafiles_ref, id, StartOnRef, EndOnRef, orient)
        % Update deltafile to represent a full alignment of the contig
        % against the reference genomen, as estimated by show-tiling.
        
        index = find(ismember({deltafiles_ref.Q},id));
        deltafiles_ref(index).S1        = StartOnRef;
        deltafiles_ref(index).E1        = EndOnRef;
        if orient == '+'
            deltafiles_ref(index).S2        = 1;
            deltafiles_ref(index).E2        = deltafiles_ref(index).LENQ;
        elseif orient == '-'
            deltafiles_ref(index).S2        = deltafiles_ref(index).LENQ;    
            deltafiles_ref(index).E2        = 1;
        else
            error('Unkown orientation');
        end
        
        deltafiles_ref(index).indels    = [];
        
    end

    function [OverlapRef, ContigRef] = UpdateOverlapRef(OverlapRef, ContigRef, id, StartOnRef, EndOnRef, orient)
        % Update deltafile to represent a full alignment of the contig
        % against the reference genomen, as estimated by show-tiling.
        
        index = find(ismember({OverlapRef.Q},id));
        
        if ~isempty(index)

            OverlapRef(index).S1        = StartOnRef;
            OverlapRef(index).E1        = EndOnRef;
            OverlapRef(index).S2        = 1;

            if orient == '+'
                OverlapRef(index).S2        = 1;
                OverlapRef(index).E2        = OverlapRef(index).LENQ;
            elseif orient == '-'
                OverlapRef(index).S2        = OverlapRef(index).LENQ;    
                OverlapRef(index).E2        = 1;
            else
                error('Unkown orientation');        
            end
            
        else
            error('Cannot find the contig in the overlap list');           
            
        end
            
    end

	function OverlapRef = filter_reverse(OverlapRef)
        
        	%for overi = 1:size(Over
        	OverlapRef = OverlapRef(cell2mat({OverlapRef.E2})-cell2mat({OverlapRef.S2})>0);
        
	end

end