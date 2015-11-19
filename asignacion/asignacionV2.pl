:- use_module(library(clpfd)).


% calcularCosto: retorna una lista con el costo de la asignacion de tareas
% para cada agente.
% ENTRADA: HL,[HC|BC],E
% SALIDA: [COS|COSTO]
% HL: un vector de  asignacion de tareas a los agentes.
% [HC|BC]: matriz de costos de asignacion a los agentes.
% [COS|COSTO]: lista en la que se guada el costo para cada agente en la asignacion.
% E: agente al que se le va a calcular el costo total en la asignacion.
calcularCosto(_,[],_,_).
calcularCosto(HL,[HC|BC],E,[COS|COSTO]):-
    my_nth(1,HL,E, L),
    sumaConsumo(L,HC,0,COS),
    NewE is E + 1,
    calcularCosto(HL,BC,NewE,COSTO).


% Funcion que me retorna una lista con las posiciones en las que 
% se encuentra un elemento en una lista determinada.
% my_nth(1,LISTA_ENTRADA,ELEMENTO, LISTA_SALIDA),
my_nth(_, [], _, []).
my_nth(N, [H|B], H, [N|R]):-
    NewN is N + 1,
    my_nth(NewN, B, H, R).
my_nth(N, [X|B], Y, R):-
    X \= Y,
    NewN is N + 1,
    my_nth(NewN, B, Y, R).

%
function([],_,[]).
function([HLP|BLP], L, [HR|BR]):-
    nth1(HLP, L, HR),
    function(BLP, L, BR).
    

% Funcion prolog: findall(R,funcion,L), me devuelve todas las posibles soluciones R
% y las guarda en un lista L.
% 				  min_list(LISTA,MINIMO), me devuelve el valos minimo de lista

% Verifica que las asignaciones dadas a los agentes no sobrepasen sus capacidades.
% R: Una asignacion de tareas.
% [C|B]: Matriz de consumo de recursos de asignaciones a  los agentes.
% E: Agente que se va a verificar que no sobrepace sus capacidad
% AC: Vector que contiene la capacidad de cada agente
verificar(_,[],_,_).  
verificar(R,[C|B],E,AC):-
    my_nth(1,R,E, L),
    
    function(L, C, LR),
    sum(LR, #=, S),
    
    nth1(E,AC,Y),
    S #=< Y,
    
    NewE is E+1,
    verificar(R,B,NewE,AC).


elegirSolucionMin(M,N,MC,AC,R):-
    % L: Contiene todas las posibles asignaciones
    %findall(R,asignacion(2,3,[[1,2,3],[3,1,2]],[3,6],R),L),
    length(R,N),
    R ins 1..M,
    
    verificar(R,MC,1,AC),
    function2(R, MCostos, C),
    

	labeling([ff, min(C)], R). %Si no se usa, la lista no queda con numeros


%Correr con elegirSolucionMin(2,3,[[1,2,3],[3,1,2]],[3,6],[[3,8,3],[4,3,9]],ASIGNACION,COSTO).
