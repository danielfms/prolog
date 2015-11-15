:- use_module(library(clpfd)).
my_nth(_, [], _, []).

my_nth(N, [H|B], H, [N|R]):-
    NewN is N + 1,
    my_nth(NewN, B, H, R).

my_nth(N, [X|B], Y, R):-
    X \= Y,
    NewN is N + 1,
    my_nth(NewN, B, Y, R).

%Me permite saber la posicion de un elemento en la lista.
indice([E|_],E,INDEX,INIT):-
    INDEX is INIT.
indice([H|B],E,INDEX,INIT):-
    NEW is INIT+1,
    indice(B,E,INDEX,NEW).
%indice([2,49,40,3,1,4,56,3,1],40,E,1).

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
	
sacarMinimo(_,[],_,_,_).
sacarMinimo([HC|COSTOS],[HL|L],ASIGNACION,COSTO,MAX):-
    HC =< MAX,
    COSTO is HC,
    ASIGNACION is HL,
    MAX is HC,
    sacarMinimo(COSTOS,L,ASIGNACION,COSTO,MAX).

elegirSolucionMin(COSTOS,L,ASIGNACION,COSTO):-
    findall(R,asignacion(2,3,[[1,2,3],[3,1,2]],[3,6],[[3,8,3],[4,3,9]],R,RR,SS,CT),L),
    mini(L,[[3,8,3],[4,3,9]],COSTOS),
    min_list(COSTOS,X),
    indice(COSTOS,X,I,1),%Tengo el indice del menor.
    nth1(I,COSTOS,COSTO),
    nth1(I,L,ASIGNACION).
    
	
   % sacarMinimo(COSTOS,L,ASIGNACION,COSTO,30).
   % elegirSolucionMin(COSTOS,L,T,I).

calcularCosto(_,[],_,_).
calcularCosto(HL,[HC|BC],[COS|COSTO],E):-
    my_nth(1,HL,E, L),
    sumaConsumo(L,HC,0,COS),
    NewE is E + 1,
    calcularCosto(HL,BC,COSTO,NewE).
%asignacion(2,3,[[1,2,3],[3,1,2]],[3,6],[[3,8,3],[4,3,9]],R,RR,SS,CT)

mini([],_,_).
mini([HL|BL],C,[COS|COSTO]):-
    calcularCosto(HL,C,CO,1),
    sum_list(CO,COS),
    mini(BL,C,COSTO).
    
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


%PROBAR elegirSolucionMin(COSTOS,L,ASIGNACION,COSTO).
