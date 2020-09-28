
node(noun,[apple]).
node(noun,[banana]).
node(verb,[ate]).
node(article, [my]).
node(article, [the]).

node(verb_phrase, [verb, noun_phrase]).
node(noun_phrase, [article, noun]).

node(sentence, [noun_phrase, verb_phrase]).

leaves(X,[X]) :- not(node(X,_)).
leaves(X,Leaves) :- node(X,List), leavesOfList(List,Leaves).

leavesOfList([],[]).
leavesOfList([H|Tail],Leaves) :-
    leaves(H,HeadLeaves),
    leavesOfList(Tail,TailLeaves),
    append(HeadLeaves,TailLeaves,Leaves).
    
