
/* Ch1 excercises */

:- discontiguous(son/3).
:- discontiguous(daughter/3).
:- discontiguous(male/1).
:- discontiguous(female/1).

male(robertO).
female(lynne).

male(george).
female(madeleine).

male(robertD).
female(carol).

son(brian, robertO, lynneO).
son(uncleBret, robertO, lynneO).
daughter(tara, robertO, lynneO).

daughter(beth, george, madeleine).
daughter(leigh, george, madeleine).
daughter(lynneW, george, madeleine).

daughter(laura, robertD, carol).
son(rob, robertD, carol).
son(kevin, robertD, carol).

son(bret, brian, beth).
son(walker, brian, beth).
son(bennett, brian, beth).

son(payton, brian, laura).
daughter(briley, brian, laura).
daughter(avery, brian, laura).

/**/

male(X) :- son(X,_,_).
female(X) :- daughter(X,_,_).

parent(P,C) :- son(C,P,_).
parent(P,C) :- daughter(C,P,_).
parent(P,C) :- son(C,_,P).
parent(P,C) :- daughter(C,_,P).

father(F,C) :- male(F), parent(F,C).
mother(M,C) :- female(M), parent(M,C).

grandparent(G,C) :- parent(G,P), parent(P,C).

grandmother(G,C) :- mother(G,P), parent(P,C).
grandfather(G,C) :- father(G,P), parent(P,C).

sameMother(X,Y) :- mother(M,X), mother(M,Y).
sameFather(X,Y) :- father(F,X), father(F,Y).

fullSibling(X,Y) :- dif(X,Y), sameMother(X,Y), sameFather(X,Y).

halfSibling(X,Y) :- dif(X,Y), sameMother(X,Y), not(sameFather(X,Y)).
halfSibling(X,Y) :- dif(X,Y), sameFather(X,Y), not(sameMother(X,Y)).

sibling(X,Y) :- fullSibling(X,Y).
sibling(X,Y) :- halfSibling(X,Y).

brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).

auntThrough(A,P,X) :- sister(A,P), parent(P,X).
aunt(A,X) :- auntThrough(A,_,X).
aunt(A,X) :- halfSibling(S,X), auntThrough(A,P,S), not(parent(P,X)).

uncleThrough(U,P,X) :- brother(U,P), parent(P,X).
uncle(U,X) :- uncleThrough(U,_,X).
uncle(U,X) :- halfSibling(S,X), uncleThrough(U,P,S), not(parent(P,X)).

love(bret,mere).
love(X,Y) :- sibling(X,Y).
love(X,Y) :- love(Y,X).


find(Var,Search,Result) :- findall(Var,Search,Xs), list_to_set(Xs,Result).

