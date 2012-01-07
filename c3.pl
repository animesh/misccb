geno(1,[1]).
geno(N,[H|T]) :- N = H , N > 1 , M is N-1 , geno(M,T).

makeList(1,[1]).
makeList(N,[L|T]) :- N=L, N > 1, N1 is N-1, makeList(N1,T).

	,
					                    cons(L1,N,L2),
							                 L is L2.

cons(X,R,[X,R]).

perm([],[]).
perm(L1,[H|T]) :- append(L11,[H|L12],L1),  append(L11,L12,L13), perm(L13,T).

diaglist1(0,[],[]).
diaglist1(N,[H1|T1],[H2|T2],) :- N>0, H2 is N-H1, M is N-1, diaglist1(M,T1,T2).

diaglist2(_,0,[],[]).
diaglist2(M,N,[H1|T1],[H2|T2],) :- N>0, N2 is M-H1, H2 is N-N2, M2 is N-1, diaglist2(M,M2,T1,T2).

nodupl([]).
nodupl([H|T]) :- \+ member(H,T), nodupl(T).

queen(N,B) :- geno(N,L1),perm(L1,B),diaglist1(N,B,L21),diaglist2(N,N,B,L22),nodupl(L21), nodupl(L22).
x|
cl([],[]).
cl([X:Y|T1],T3) :- cl(T1,T2),
	(
	 member(X,T2) -> (member(Y,T2) -> T3 = T2 ; T3 = [Y|T2]);
	 (member(Y,T2) -> T3 = [X|T2] ; append([X|Y],T2,T3));
	 ).

no(X,Y,M) :- member((X:Y),M).
no(X,Y,M) :- member((Y:X),M).

guess(_,[],[]).
guess(P,[CO|M],Cl) :- member(COL,P), guess(P,M,C2), Cl=[(CO:COL)|C2].

colof(C1,C2,C3) :- member((C1:C3),C2).

crash([],_,_) :- fail.
crash([Co1|_],C,M) :- no(Co1,Co2,M), colof(Co2,C,C12), Cl1=Cl2, !.
crash([_|Cs],C,M) :- crash(Cs,C,M).

col(P,M,C) :- cl(M,L),guess(P,L,C), \+ crash(L,C,M).


	 
	
cfa(_,_,[]).
cfa(D,E,[H|T]):-DN is D+1, \+ (pt(DN,E,H), cfa(DN,E,T)).

pt(_,P,P).
pt(D,P,E) :- E is P+D.
pt(D,P,E) :- E is P-D.



			      