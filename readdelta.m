function [d multiple_align] = readdelta(filename)
% Reads delta-file from disk (nucmer output)
%
% INPUT Filename
%
% OUTPUT
% Struct with fields:
%     R             Reference contig
%     Q             Query contig
%     LENR          Length of reference contig
%     LENQ          Length of query contig
%     S1            Start position of alignment on reference
%     E1            End position of alignment on reference
%     S2            Start position of alignment on query
%     E2            End position of alignment on query
%     error
%     sim_error
%     stop_codon
%     indels

    % Count number of alignments
    [s w] = unix(['grep "^0" ' filename  ' | wc -l'] );
    if s ~= 0
      error(['Unable to read deltafile, error: ' num2str(s)]);
    elseif length(str2num(w)) > 1
      error(['length of file is not a scalar: ' num2str(w) 'while trying to read ' filename]); 
    else
      w = str2num(w);
    end

    % Initialize struct
    d = struct('R',[],'Q',[],'LENR',[], 'LENQ', [] , 'S1' ,[], 'E1' ...
    ,[],'S2',[],'E2',[],'error',[],'sim_error',[],'stop_codon',[]);

    if w == 0
    	multiple_align = containers.Map;
        d = [];
        return;
    end
       

    d(w).stop_codon = [];


    % Hash table to save alignments that vave multiple hits. Key is pair of
    % contigs, value is locations of this pair in the delta array
    multiple_align = containers.Map;
    
    i = 1;
    
    fid = fopen(filename);
    line = 1;
    
    % Get line
    line = fgetl(fid);
    
    % Parse the file
    while ~isequal(line,-1)
        %fprintf('%s\n',line);
        
        % New alignment header
        if line(1) == '>'
            
            % Read first header
            v = textscan(line,'>%s %s %d %d');
            d(i).R = char(v{1});
            d(i).Q = char(v{2});
            d(i).LENR = v{3};
            d(i).LENQ = v{4};
            
            line = fgetl(fid);
            while ~isequal(line,-1)
                % Read second header
                v = textscan(line,'%d %d %d %d %d %d %d');
                d(i).S1 = v{1};
                d(i).E1 = v{2};
                d(i).S2 = v{3};
                d(i).E2 = v{4};
                d(i).error = v{5};
                d(i).sim_error = v{6};
                d(i).stop_codon = v{7};

                % Read indel positions
                indels = [];
                line = fgetl(fid);
            

                while ~isequal(line,'0')
                    indels = [indels sscanf(line,'%d')];
                    line = fgetl(fid);
                end      
                d(i).indels = indels;
                i = i+1;
                
                line = fgetl(fid);
                if isequal(line,-1) || isequal(line(1),'>')
                    
                    % Add to the list with multiple alignments
                    if isKey(multiple_align, [d(i-1).R d(i-1).Q])
                        add_to_multiple_align(multiple_align,d(i-1).R,d(i-1).Q,i-1);
                    end
                    break
                else
                    % Add to the list with multiple alignments
                    add_to_multiple_align(multiple_align,d(i-1).R,d(i-1).Q,i-1);
                    d(i).R = d(i-1).R;
                    d(i).Q = d(i-1).Q;
                    d(i).LENR = d(i-1).LENR;
                    d(i).LENQ = d(i-1).LENQ;
                end
                

            end    
            
        else
            line = fgetl(fid);
        end
    end
    
    fclose(fid);
    
   
    
    function add_to_multiple_align(multiple_align,R,Q,loc)
        
        if isKey(multiple_align, [R Q])
            v = multiple_align([R Q]);
            v = [v loc];
            multiple_align([R Q]) = v;
        else
            multiple_align([R Q]) = loc;
        end
    end
    
end
