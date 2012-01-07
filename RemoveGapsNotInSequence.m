function am = RemoveGapsNotInSequence(am)
% Remove dots representing gaps that are not in a sequence in the alignment
% matrix 'am'.

% Loop through alignment matrix
for r = 1:size(am,1)
    InSeq = false;
    for c = 1:size(am,2)
        if InSeq
            % Check for end of sequence
            if am(r,c) == ' ' || am(r,c) == 0
                InSeq = false;
            end
        else
            % Not in sequence -> remove dot
            if am(r,c) == '.'
                am(r,c) = ' ';

            % Check for start of new sequence
            elseif am(r,c) ~= ' ' && am(r,c) ~= 0
                InSeq = true;
            end
        end
    end
end
    