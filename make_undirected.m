function Gu = make_undirected(G)
    Gt = G';
    Gu = G;
    Gu(G==0) = Gt(G==0);
end