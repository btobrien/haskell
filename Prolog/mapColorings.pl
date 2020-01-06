
adj(1,2).
adj(1,3).
adj(1,4).
adj(1,5).
adj(2,3).
adj(2,4).
adj(3,4).
adj(4,5).
adj(2,1).
adj(3,1).
adj(4,1).
adj(5,1).
adj(3,2).
adj(4,2).
adj(4,3).
adj(5,4).

color(1,red,a).
color(2,blue,a).
color(3,green,a).
color(4,yellow,a).
color(5,blue,a).

color(1,red,b).
color(2,blue,b).
color(3,green,b).
color(4,blue,b).
color(5,green,b).

conflict(Coloring) :-
    adj(X,Y),
    color(X,Color,Coloring),
    color(Y,Color,Coloring).

conflict(R1,R2,Coloring) :-
    adj(R1,R2),
    color(R1,Color,Coloring),
    color(R2,Color,Coloring).


