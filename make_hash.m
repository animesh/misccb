function h = make_hash(names)
    % Build hash table of positions of contigs in unique names matrix
    hi = {};
    for i = 1:length(names), hi{i} = i;end; % cell arrar with all positions
    h = containers.Map(names, hi);
end
