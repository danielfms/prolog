:- use_module(library(clpfd)).
my_nth(_, [], _, []).

my_nth(N, [H|B], H, [N|R]):-
    NewN is N + 1,
    my_nth(NewN, B, H, R).

my_nth(N, [X|B], Y, R):-
    X \= Y,
    NewN is N + 1,
    my_nth(NewN, B, Y, R).

sumaConsumo([] , _, V, S):-
    S is V.

%sumaConsumo([1,3],[1,2,3,4],0,S).

sumaConsumo([LC|L], C, V, S):-
    nth1(LC, C, Y),
    NewV is V + Y,
    sumaConsumo(L, C, NewV, S).


asignacion(M,N,CB,AC,MC,R,RR,SS,CT):-
    length(R,N),
    R ins 1..M,
    labeling([ff], R),
    verificar(R,CB,1,AC,RR,MC,SS),
    sum_list(SS,CT).
%asignacion(2,4,R).
%
%asignacion(2,3,[[1,2,3],[3,1,2]],[6,6],R).
%asignacion(2,3,[[1,2,3],[3,1,2]],[3,6],[[3,8,3],[4,3,9]],R,RR,SS,CT).

verificar(_,[],_,_,[],[],[]).  
%verificar([1,2,1],[[1,1,1],[1,1,1]],1,[6,6],R)
verificar(R,[C|B],E,AC,[CS|BS],[MC|MCC],[SCS|SCC]):-
    my_nth(1,R,E, L),
    sumaConsumo(L,C,0,S),
    sumaConsumo(L,MC,0,SC),
    SCS is SC,
    CS is S,
    nth1(E,AC,Y),
    NewE is E+1,
    S =< Y,
    verificar(R,B,NewE,AC,BS,MCC,SCC).

%PROBAR CON asignacion(2,3,[[1,2,3],[3,1,2]],[3,6],R,RR).
