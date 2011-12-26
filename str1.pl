alignment([],[],[]).

alignment([X|Xs],[X|Ys],[X|A]):-alignment(Xs,Ys,A).

alignment([_|Xs],Ys,A):- alignment(Xs,Ys,A).

alignment(Xs,[_|Ys],A):- alignment(Xs,Ys,A).



listlen([],0).

listlen([H|T],N):- listlen(T,N1),N is N1 + 1.




maximum([X],X).
maximum([X|Xs],Max):- M = X,maxi(Xs,M,Max).


maxi([],Max,Max).
maxi([Y|Xs],M,Max):- listlen(M,N1),
		listlen(Y,N),
		N1<N,!,
		maxi(Xs,Y,Max).

maxi([Y|Xs],M,Max):-maxi(Xs,M,Max).



maxAlignment(Xs,Ys,Alignment):-findall(S,alignment(Xs,Ys,S),L),
				maximum(L,Alignment).

