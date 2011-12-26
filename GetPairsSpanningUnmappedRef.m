function pairs = GetPairsSpanningUnmappedRef(ContigRef, OverlapRef, NameHashRef, RefFasta, clipping_thrs, MinGapSize, MaxGapSize, AllowReverse) 
%
% INPUT
% AllowReverse:    BOOLEAN True: reverse alignment on reference allowed


% Filter out reverse alignments to the reference
if ~AllowReverse
    OverlapRef = filter_reverse(OverlapRef);
end


PosMat      = GetPositionMatrix(ContigRef,OverlapRef, NameHashRef, clipping_thrs);

UnMappedMat = GetUnmappedRefPos(PosMat,RefFasta);

[UnUsed,refseq]  = fastaread(RefFasta);

pairs       = FindPairsAroundGaps(OverlapRef, PosMat, UnMappedMat, MinGapSize, MaxGapSize, clipping_thrs, length(refseq));


    function OverlapRef = filter_reverse(OverlapRef)
        
        %for overi = 1:size(Over
        OverlapRef = OverlapRef(cell2mat({OverlapRef.E2})-cell2mat({OverlapRef.S2})>0);
        
    end


    function pm = GetPositionMatrix(ContigRef,OverlapRef, NameHashRef, clipping_thrs)

        % Initialize PosMat
        pm = zeros(size(OverlapRef,2), 5);
        
        % Make reference to index in OverlapRef to find back entry after sorting and cutting
        pm(:,5) = 1:size(OverlapRef,2);        
        
        % Loop through all overlaps
        for oi = 1:size(OverlapRef,2)
            
            % Make table with [S1 E1 5' 3']
            % Where 5' == 1 means that the downsteam end of the contig aligns
            % to the reference with overhang < max_clipping
            [upstream downstream] = down_or_upstream(OverlapRef, oi, NameHashRef, ContigRef, clipping_thrs);
            pm(oi,1:4)              = [OverlapRef(oi).S1 OverlapRef(oi).E1 double(upstream) double(downstream)];
            
        end
               
        % Filter alignments that have too much overhang on both sides
        TooMuchOverhang = ((pm(:,3) == 0) .* (pm(:,4) == 0));
%        OverlapRef      = OverlapRef(:,~TooMuchOverhang);
        pm              = pm(~TooMuchOverhang,:);
        
        
        
    end


    function UnMappedMat = GetUnmappedRefPos(PosMat,RefFasta)
        
        reflen = GetRefLength(RefFasta);
        
        templ  = zeros(1, reflen);
        
        for p = 1:size(PosMat, 1)
            templ(PosMat(p,1):PosMat(p,2)) = 1;
        end
        
        % Initialize maximum possible matrix for gaps
        UnMappedMat = zeros(int32((reflen/2))+1,2);
        UMM = 1;
        
        r = 1;
        while r <= reflen
            
            % Check for start of gap
            if templ(r) == 0
                
                % If so, mark start of gap
                UnMappedMat(UMM,:) = [r 0];

                % Loop through the gap
                while r <= reflen && templ(r) == 0
                r = r+1;
                end

                % Mark the end of the gap
                if r == reflen 
                 UnMappedMat(UMM,2) = r;
                else           
                 UnMappedMat(UMM,2) = r-1;
                end

                % Next row for next gap
                UMM = UMM + 1;
                
            else
                r = r+1;
            end
            
        end
        
        % Resize matrix
        UnMappedMat = UnMappedMat(1:UMM-1,:);
    end


    function L = GetRefLength(RefFasta)
        [UnUsed, seq] = fastaread(RefFasta);
        L = length(seq);
    end


    function pairs = FindPairsAroundGaps(OverlapRef, PosMat, UnMappedMat, MinGapSize, MaxGapSize, clipping_thrs,LenRefFasta)

        % pairs                 [node1 node2 A1 A2 A3 A4]
    
        %                ---------------       -----------------
        %                          |||||       ||||||
        % ------------------------------------------------------------------------------
        %                          |   |       |    |
        %                          A3  A1      A2   A4
        
        pairs    = [];
	RefStart = 1;
	RefEnd   = LenRefFasta;        

        % Split PosMat in downstream and upstream alignments (contigs can
        % be in both of these matrices)
        PosMatUp   = PosMat(PosMat(:,3) == true,:);
        PosMatDown = PosMat(PosMat(:,4) == true,:);
        
        % Sort upstream matrix on S1
        [UnUsed,index]  = sort(PosMatUp(:,1));
        PosMatUp   = PosMatUp(index,:);
                
        % Sort downstream matrix on E1
        [UnUsed,index]  = sort(PosMatDown(:,2));
        PosMatDown = PosMatDown(index,:);
        
        
        % Loop through gaps and find pairs spanning the gaps
        for g = 1:size(UnMappedMat,1)
            
	    % Exclude 'gaps' at start and end of a chromosome
 	    if UnMappedMat(g,1) == RefStart || UnMappedMat(g,2) == RefEnd
		continue;
	    end

            % Test Gapsize
            GapSize = UnMappedMat(g,2) - UnMappedMat(g,1) + 1;
            if (GapSize >= MinGapSize) && (GapSize <= MaxGapSize)
                
                % Find aligned contigs closest to gap
                % Find contigs that connect their upstream end to the gap
                UpContigs     = PosMatUp(PosMatUp(:,1) > UnMappedMat(g,2),:);
		if ~isempty(UpContigs)
			Closest_index = UpContigs(1,5);
		end
                UpContigs     = UpContigs((UpContigs(:,1) - UnMappedMat(g,2)) <= clipping_thrs,:);
                UpIndices     = UpContigs(:,5);
		
		if isempty(UpIndices)
			UpIndices = Closest_index;
		end
                
                DownContigs   = PosMatDown(PosMatDown(:,2) < UnMappedMat(g,1),:);
		if ~isempty(DownContigs)
			Closest_index = DownContigs(1,5);
		end
                DownContigs   = DownContigs((UnMappedMat(g,1) - DownContigs(:,2)) <= clipping_thrs,:);
                DownIndices   = DownContigs(:,5);                

		if isempty(DownIndices)
			DownIndices = Closest_index;
		end                

                for pUp = 1:length(UpIndices)
                    for pDown = 1:length(DownIndices)
                        % Add pairs to the list
                        pairs = [pairs; {OverlapRef(DownIndices(pDown)).Q OverlapRef(UpIndices(pUp)).Q ...
                                         OverlapRef(DownIndices(pDown)).E1 OverlapRef(UpIndices(pUp)).S1 ...
                                         OverlapRef(DownIndices(pDown)).S1 OverlapRef(UpIndices(pUp)).E1}];   
                    end
                end
            end
        end
    end

    function [U D] = down_or_upstream(overlap, nr, name_hash, contig, clipping_thrs)
        % Check which end of a contig maps to the reference
        % This information is needed to know whether to look up or
        % downstream for a matching contig to connect via the reference
        
        D = false; % Downstream
        U = false; % Upstream
        
        if overlap(nr).S2 - clipping_thrs <= 0 || (contig(name_hash(overlap(nr).Q)).size - overlap(nr).S2) <= clipping_thrs
            % Is 5' end of contig is aligned to reference, then
            % check upstream of chromosome if it can be
            % connected ti contig 'oj'
            U = true;
        end

        % Check if the rev comp is aligned     ||  Check if the straight contig complement is aligned             
        if overlap(nr).E2 - clipping_thrs <= 0 || (contig(name_hash(overlap(nr).Q)).size - overlap(nr).E2) <= clipping_thrs
            % Is 3' end of contig is aligned to reference, then
            % check downstream of chromosome if it can be connected to
            % contig 'oj'
            D = true;
        end
    end
end
