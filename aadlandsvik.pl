% Solution to problem A
% - uses the built-in predicate between/3 to generate permutations

% checks that no queens are on the same north-east diagonal
diagonal_ne(_, []).
diagonal_ne(X, [H|T]) :- Z is X + 1, Z \== H, diagonal_ne(Z, T).

% checks that no queens are on the same south-east diagonal
diagonal_se(_, []).
diagonal_se(X, [H|T]) :- Z is X - 1, Z \== H, diagonal_se(Z, T).

% checks that no queens are on the same horizontal line
horizontal(_, []).
horizontal(X, [H|T]) :- X \== H, horizontal(X, T).

% generates a valid chessboard of width X and height Y with one queen in each column
queen(_, 0, []).
queen(Y, X, [H|T]) :- X > 0, Z is X - 1, queen(Y, Z, T), between(1, Y, H), horizontal(H, T), diagonal_ne(H, T), diagonal_se(H, T).

queen(N, L) :- queen(N, N, L).


% Solution to problem B
% - uses the built-in predicates append/3 and member/2

% conditional append, only appends if E is not in the list L
cond_append(L, E, M) :- not(member(E, L)), !, append(L, [E], M).
cond_append(L, _, L).

% extracts countries from the neighbor map and puts them into a list
find_countries([], []).
find_countries([A:B|T], N) :- find_countries(T, L), cond_append(L, A, M), cond_append(M, B, N).

% makes permutations as a country:color list with the specified country and color lists
make_perm(_, [], []).
make_perm(C, [H|T], [H:P|O]) :- make_perm(C, T, O), member(P, C).

% checks if the map coloring is valid, that is - no equal colors beside each other
ok_colors([], _).
ok_colors([A:B|T], P) :- ok_colors(T, P), member(A:C, P), member(B:D, P), C \== D.

% generates valid colorings of a map if they exist
col(C, M, P) :- find_countries(M, N), make_perm(C, N, P), ok_colors(M, P).
