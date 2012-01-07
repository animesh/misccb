function [path] = build_path(pred, v)
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

    path = [v];
    while pred(v) ~= 0,
        v = pred(v);
        path = [v path];
    end
end


