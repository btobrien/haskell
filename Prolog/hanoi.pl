
move(1,X,_,Z) :- write(X), write(Z).

move(N,X,Y,Z) :-
    M is N-1,
    move(M,X,Z,Y),
    move(1,X,_,Z),
    move(M,Y,X,Z).

main :-
    current_prolog_flag(argv, Argv), nth0(0, Argv, A0),
    atom_number(A0, Start),
    move(Start,j,k,l), write('\n').
