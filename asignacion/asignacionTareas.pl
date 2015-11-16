:- use_module(library(clpfd)).

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

%Me permite saber la posicion de un elemento en la lista.
%indice(LISTA,ELEMENTO_A_BUSCAR,INDICE,INIT)
%SALIDA: INDICE
%INIT debe de ser 1, para que comience a buscar desde la posicion 1.
%indice([2,49,40,3,1,4,56,3,1],40,INDICE,1): INDICE:3.
indice([E|_],E,INDEX,INIT):-
    INDEX is INIT.
indice([H|B],E,INDEX,INIT):-
    NEW is INIT+1,
    indice(B,E,INDEX,NEW).

% Permite sumar determinados elementos dados por una lista de posiciones
% en una lista dada.
% sumaConsumo(POSICIONES_A_SUMAR[LISTA],LISTA_DE_ELEMENTOS,INIT,SUMA).
% INIT debe de comenzar en cero, ya que es la inicialización de la SUMA.
%sumaConsumo([1,3],[1,2,3,4],0,S).
%
% nth1(INDICE,LISTA,ELEMENTO): Retorna el elemeno que esta en la 
% posición INDICE en LISTA.
sumaConsumo([] , _, V, S):-
    S is V.
sumaConsumo([LC|L], C, V, S):-
    nth1(LC, C, Y),
    NewV is V + Y,
    sumaConsumo(L, C, NewV, S).

% Verifica que las asignaciones dadas a los agentes no sobrepasen sus capacidades.
% R: Una asignacion de tareas.
% [C|B]: Matriz de consumo de recursos de asignaciones a  los agentes.
% E: Agente que se va a verificar que no sobrepace sus capacidad
% AC: Vector que contiene la capacidad de cada agente
verificar(_,[],_,_).  
verificar(R,[C|B],E,AC):-
    my_nth(1,R,E, L),
    sumaConsumo(L,C,0,S),
    nth1(E,AC,Y),
    NewE is E+1,
    S =< Y,
    verificar(R,B,NewE,AC).

% asignacion:Retorna una asignación de tareas.
% ENTRADA: M,N,CB,AC
% SALIDA: R
% M: numero de agentes
% N: numero de tareas
% MC: Matriz de consumo de recursos de las tareas asignadas a los agentes
% AC: Vector que contiene la capacidad de cada agente
% R: Una asignacion de tareas a los agentes
% 
% Funciones prolog: length(R,N), crea una lista R de N posiciones
% 					R ins 1..M, llena R de valores entre 1 y M.
% 					labeling([ff], R), le da formato a R
asignacion(M,N,MC,AC,R):-
    length(R,N),
    R ins 1..M,
    labeling([ff], R), %Si no se usa, la lista no queda con numeros
    verificar(R,MC,1,AC).

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

%mini: Calcula el costo de las asignaciones posibles, esto sirve para luego
%determinar cual es la de menor costo, ya que esta es la mejor solucion.
% [HL|BL]: Lista de listas, contiene todas las asignaciones posibles.
% C: Matriz de costos de asignaciones a los agentes.
% [COS|COSTO]: Vector que contiene el costo total de cada asignacion.
% 
% Funciones prolog: sum_list(LISTA, SUMA): Suma todos los elementos de una lista
mini([],_,_).
mini([HL|BL],C,[COS|COSTO]):-
    calcularCosto(HL,C,1,CO),
    sum_list(CO,COS),
    mini(BL,C,COSTO).

%elegirSolucionMin: Calcula todas las posibles asignaciones y de esta determina
%cual me minimiza el costo.
% SALIDAS: ASIGNACION, COSTO
% M: Numero de agentes
% N: Numero de tareas
% ASIGNACION: Mejor asignacion que minimiza el costo
% COSTO: Costo de la mejor asignación.
% 
% Funcion prolog: findall(R,funcion,L), me devuelve todas las posibles soluciones R
% y las guarda en un lista L.
% 				  min_list(LISTA,MINIMO), me devuelve el valos minimo de lista

elegirSolucionMin(M,N,MATRIZ_CONSUMO,VECTOR_CAPACIDADES,MATRIZ_COSTOS,ASIGNACION,COSTO):-
    % L: Contiene todas las posibles asignaciones
    %findall(R,asignacion(2,3,[[1,2,3],[3,1,2]],[3,6],R),L),
    findall(R,asignacion(M,N,MATRIZ_CONSUMO,VECTOR_CAPACIDADES,R),L),
    %mini(L,[[3,8,3],[4,3,9]],COSTOS), %Costos totales de cada asignacion
    mini(L,MATRIZ_COSTOS,COSTOS), %Costos totales de cada asignacion
    min_list(COSTOS,X),
    indice(COSTOS,X,I,1),%Tengo el indice del menor.
    nth1(I,COSTOS,COSTO),%Obtengo el menor costo
    nth1(I,L,ASIGNACION).%Obtengo la asignacion del menor costo

%Correr con elegirSolucionMin(2,3,[[1,2,3],[3,1,2]],[3,6],[[3,8,3],[4,3,9]],ASIGNACION,COSTO).
