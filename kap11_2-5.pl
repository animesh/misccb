% Løsningsforslag til oppgaver i INF121, høsten 2009
% Uke 43, Prolog

% Programing in prolog, kap 1
% øvelse 1: først vil prolog unersøke om likes(john,wine) kan tilfredsstilles flere ganger, siden det ikke går, vil den undersøke om likes(mary,X) kan tilfredsstilles flere ganger, og siden det heller ikke går vil den avslutte med No eller fail.

% øvelse 2. Svarene som kan fås er sister_of(alice, edward) og sister_of(alice,alice). Det første er beskrevet i teksten. I det siste vil både X og Y unifiseres med alice, som er helt greit siden det ikke er skrevet X \= Y.

%øvelse 3
is_mother(X) :- mother(X,_).
is_father(X) :- father(X,_).
is_son(X) :- male(X),parent(_,X).
% Jeg teller med halvsøsken
sister_of(X,Y) :- X \= Y, female(X), parent(P,X),parent(P,Y).
grandpa_of(X,Y) :- parent(P,Y),parent(X,P).
sibling(X,Y) :- X \= Y,parent(P,X),parent(P,Y).
	
% øvelse 4 Ved å kreve at de ikke er like, som vi har gjort over. Merk at predikatet diff tilsvarer \= , som er innebygd i swi-prolog.


% Sethi kap 11

%11.2

%a X is third element of Xs
third(X,Xs) :- append([_,_],[X|_],Xs).

% a uten append
third2(X,[_,_,X|_]).

%b X is the last element of Xs
last(X,Xs) :- append(_,[X],Xs).

%c Hd er alle unntatt siste element i Xs
head(Hd,Xs) :- append(Hd,[_],Xs).

%d True iff Xs is a concatenation of a sublist with itself three times
trippel(Xs) :- append(Ys,Ys2,Xs),append(Ys,Ys,Ys2).

%Merk at følgende predikat ikke terminerer hvis Xs er gitt og ikke oppfyller kriteriene. Hvorfor?
%trippel(Xs) :- append(Ys,Ys,Ys2),append(Ys2,Ys,Xs).

%e listen Y er dannet ved å legge inn element A et sted i listen X
inserta(Y,A,X) :- append(X1,[A|X2],Y),append(X1,X2,X).

%e tilsvarende predikat uten append
insert2([A|X],A,X).
insert2([H|Y],A,[H|X]) :- insert2(Y,A,X).

%11.3
%a sann hvis og bare hvis lista Xs er en permutasjon (omstokking?) av lista Ys
permutasjon([],[]).
permutasjon([X|Xs],Ys) :- permutasjon(Xs,Ys2),insert2(Ys,X,Ys2).

%b sann hvis og bare hvis argumentet er ei liste som har jevnt antall elementer
jevn([]).
jevn([_,_|Xs]) :- jevn(Xs).

%c er lista som er det tredje argumentet satt sammen av elementene i listene som er de to første arguementene?
merge([],Ys,Ys).
merge([X|Xs],Ys,Zs) :- insert(Zs,X,Zs2),merge(Xs,Ys,Zs2).

% Alternativ som bruker permutasjon
merge2(Xs,Ys,Zs) :- append(Xs,Ys,XY),permutasjon(XY,Zs).

%d sann hvis og bare hvis lista er et palindrom
palindrome([]).
palindrome([_]).
palindrome([X|Xs]) :- append(Ys,[X],Xs),palindrome(Ys).

% Alternativ uten append. Mer effektiv?
pal2(Xs) :- palf(Xs,[]).
palf([X|Xs],Ys) :- palf(Xs,[X|Ys]).
palf(Xs,Xs).
palf([_|Xs],Xs).

%11.4 Sammenlikn gjerne med oppgave 2 fra ovelserkap9sethi, som ble gjennomgått i uke 40

%a Fjern den andre og alle påfølgende kopier i Xs - dette skal være Ys

firea([],[]).
firea([X],[X]).
firea([X,X|Xs],Ys) :- firea([X|Xs],Ys).
firea([X,Y|Xs],[X|Ys]) :- X \= Y, firea([Y|Xs],Ys).
% Merk at her kunne vi satt et cut i linje 3 i stedet for ulikheten i linje 4.

%b La bare elementer som ikke har kopier være i Ys
fireb([],[]).
fireb([X],[X]).
fireb([X,X|Xs],Ys) :- !, fjern(Xs,X,Xs2), fireb(Xs2,Ys).
fireb([X,Y|Xs],[X|Ys]) :- X \= Y, fireb([Y|Xs],Ys).

%hjelpe predikat for b og c. fjern(Xs,X,Ys) fjerner alle elementer X som er i begynnelesen av Xs
fjern([X|Xs],X,Ys) :- !,fjern(Xs,X,Ys).
fjern(Xs,_,Xs).

%c La bare en kopi være igjen av elementer som har duplikater
firec([],[]).
firec([_],[]).
firec([X,X|Xs],[X|Ys]) :- !,fjern(Xs,X,Xs2),firec(Xs2,Ys).
firec([X,Y|Xs],Ys) :- X \= Y, firec([Y|Xs],Ys).

%11.5 insert(K,T1,T2) lykkes hvis T2 er T1 med K innsatt på riktig plass
% Det skjer ingen "balansering" av søketreeet.
% Hvis verdien allerede finnes i søketreeet, skjer det ingen endring 
% Søketrærne er beskrevet i side 16 av notatene LP1.pdf
insert(V,e,node(V,e,e)).
insert(V,node(V,T1,T2),node(V,T1,T2)) :- !.
insert(V,node(K,T1,T2),node(K,T1I,T2)) :- V < K, !, insert(V,T1,T1I).
insert(V,node(K,T1,T2),node(K,T1,T2I)) :- V > K, !, insert(V,T2,T2I).

% Fjerner en verdi fra treet. Den fjernede verdien erstattes med verdien som er lengst til høyre i det venstre deltreet.
delete(V,node(V,e,T),T).
delete(V,node(V,T1,T2),node(T1H,T1R,T2)) :- deleteHigh(T1,T1H,T1R).
delete(V,node(K,T1,T2),node(K,T1D,T2)) :- V < K, !, delete(V,T1,T1D).
delete(V,node(K,T1,T2),node(K,T1,T2D)) :- V > K, !, delete(V,T2,T2D).

% hjelpe-predikat for delete. Finner noden med høyest verdi, og fjerner denne fra treet. Dette er noden som er lengst til høyre, så den har ingen barn til høyre.
deleteHigh(node(V,T1,e),V,T1) :- !.
deleteHigh(node(K,T1,T2),V,node(K,T1,T2R)) :- deleteHigh(T2,V,T2R).


