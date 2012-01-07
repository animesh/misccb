function ov = GetOverlap(overlap, name1, name2)

ov = overlap(arrayfun(@(x) (isequal(x.R,name1) || isequal(x.Q, name1)), overlap));

if nargin > 2
    ov = ov(arrayfun(@(x) (isequal(x.R,name2) || isequal(x.Q, name2)), ov));
end

