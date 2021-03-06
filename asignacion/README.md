#Problema de Asignacion de un conjunto de trabajos a un conjunto de agentes que minimice el costo de las asignaciones.

#asignacionTareas.pl
#elegirSolucionMin(M,N,MATRIZ_CONSUMO,VECTOR_CAPACIDADES,MATRIZ_COSTOS,ASIGNACION,COSTO)

PROBLEMA:
Asignar un conjunto de trabajos a un conjunto de agentes sujeto a dos restricciones:
	-Cada trabajo debe ser asignado exactamente a un agente.
	-El número de recursos consumidos por todos los trabajos asignados a un agente no debe superar la capacidad del agente.

La entrada del problema se puede describir por los siguientes parámetros:
	-M: número de agentes
	-N: número de trabajos
	-A: Una matriz de dimensiones MxN. De tal modo que A(i,j) contiene el consumo de recurso al asignar al agente i para realizar el trabajo j.
	-B: Un vector de dimensión M que contiene la capacidad de cada agente. B[i] es la capacidad(recursos).
	-C: Una matriz de dimensiones MxN. C(i,j) es el costo de asignar el trabajo j al agente i.

Encontrar aquella solución que provea la asignación más economica teniendo en cuenta los costos de asignación en C.


ENTRADA: M,N,MATRIZ_CONSUMO, VECTOR_CAPACIDADES, MATRIZ_COSTOS.
SALIDA: ASIGNACION, COSTO.

Ejemplo 1:
M = 2
N = 3
A = MATRIZ_CONSUMO(MxN) = [[1,2,3],[3,1,2]]
B = VECTOR_CAPACIDADES(Mx1) = [3,6]
C = MATRIZ_COSTOS(MxN) = [[3,8,3],[4,3,9]]

Correr: elegirSolucionMin(2,3,[[1,2,3],[3,1,2]],[3,6],[[3,8,3],[4,3,9]],ASIGNACION,COSTO).
Salida: ASIGNACION = [2, 2, 1],COSTO = 10.

Ejemplo 2:
M = 5
N = 15
A = MATRIZ_CONSUMO(MxN) = [[8,15,14,23,8,16,8,25,9,17,25,15,10,8,24],
			   [15,7,23,22,11,11,12,10,17,16,7,16,10,18,22],
			   [21,20,6,22,24,10,24,9,21,14,11,14,11,19,16],
			   [20,11,8,14,9,5,6,19,19,7,6,6,13,9,18],
			   [8,13,13,13,10,20,25,16,16,17,10,10,5,12,23]]
B = VECTOR_CAPACIDADES(Mx1) = [36,34,38,27,33]
C = MATRIZ_COSTOS(MxN) = [[17,21,22,18,24,15,20,18,19,18,16,22,24,24,16],
			  [23,16,21,16,17,16,19,25,18,21,17,15,25,17,24],
			  [16,20,16,25,24,16,17,19,19,18,20,16,17,21,24],
			  [19,19,22,22,20,16,19,17,21,19,25,23,25,25,25],
			  [18,19,15,15,21,25,16,16,23,15,22,17,19,22,24]]
Correr: elegirSolucionMin(5,15,[[8,15,14,23,8,16,8,25,9,17,25,15,10,8,24],[15,7,23,22,11,11,12,10,17,16,7,16,10,18,22],[21,20,6,22,24,10,24,9,21,14,11,14,11,19,16],[20,11,8,14,9,5,6,19,19,7,6,6,13,9,18],[8,13,13,13,10,20,25,16,16,17,10,10,5,12,23]],[36,34,38,27,33],[[17,21,22,18,24,15,20,18,19,18,16,22,24,24,16],[23,16,21,16,17,16,19,25,18,21,17,15,25,17,24],[16,20,16,25,24,16,17,19,19,18,20,16,17,21,24],[19,19,22,22,20,16,19,17,21,19,25,23,25,25,25],[18,19,15,15,21,25,16,16,23,15,22,17,19,22,24]],ASIGNACION,COSTO).
Salida: ASIGNACION = [] ,COSTO = .
