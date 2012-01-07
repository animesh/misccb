path(X,X, _).
path(Start, Target, Edges):- go(Start, Target,Edges, [Start]).

go(S,S,_,_).
go(S,T,Xs,Brukt):- member((S,X),Xs),
			\+ member(X,Brukt),
			go(X,T,Xs,[S|Brukt]).
