:- use_module(library(clpfd)).
:- include('futoshiki.pl').

%genera cuadrados latinos de dimension N, lo devuelve en Rows
gen_latinsq(N, Rows) :-
	N1 is N - 1,
	I is random(N1),	  	%0..N-1
	J is random(N1),	  	%0..N-1
	C is random(N1) + 1,  	%1..N
	cell([I,J], Rows, C), 	%random seed
    length(Rows, N), maplist(length_(N), Rows),
    append(Rows, Cells), Cells ins 1..N,
    latin_square(Rows),
    label(Cells).		

gen_solution(N, Rows) :-
	once(gen_latinsq(N,Rows)).

%variaciones con repeticion del conjunto Vars, tomados sus elementos de 2 en 2 (i,j)
varia_rep(0,[]).
varia_rep(N,Vars):-
	N1 is N-1,
	length(Vars,2), %[I,J]
	Vars ins 0..N1,
	label(Vars).

%genera la lista de pares [i,j] de posiciones permitidas en una matriz de N x N
gen_positions(N,LPos) :-
	findall(Pos, varia_rep(N,Pos), LPos).

%genera una matriz vacia de dimensiones N x N
empty_problem(N, Rows) :-
	length(Rows, N),
	maplist(length_(N), Rows).

%devuelve posiciones de celdas adyacentes que cumplen la restriccion de menor estricto
gen_restrictions(N, Rows, Vars) :-
	N1 is N - 1,
	Vars = [I1, J1, I2, J2],
	Vars ins 0..N1,
	%si solo se diferencian en 1 fila y tienen la misma columna o viceversa
	((J1 #= J2) #/\ ((I1 #= I2 - 1) #\/ (I1 #= I2 + 1))) #\/ ((I1 #= I2) #/\ ((J1 #= J2 - 1) #\/ (J1 #= J2 + 1))), 	   
	unequal(Rows, [I1,J1,I2,J2]).

%genera todas las restricciones de menor estricto entre cada par de elementos de la matriz
gen_lessthans(N, Rows, Lts) :-
	findall(Restr, gen_restrictions(N, Rows, Restr), Lts).

%unifica el valor de la celda [i,j] de Rows con la celda [i,j] de Rows1 y elimina dicha posicion para no volverla a tomar
set_cell(Rows, LPos, Rows1, LPos1) :-
	random_member(P, LPos), %Obtiene P = [i,j] aleatoriamente de LPos
	delete(LPos, P,LPos1),
	cell(P, Rows, C),
    cell(P, Rows1, C).

%obtiene una restriccion aleatoria de Lessthans, quitandola para no volverla a tomar
get_lessthans(Lts, LtsR, Restr) :-
	random_member(Restr, Lts),
	delete(Lts, Restr, LtsR).

%LPos: lista de posiciones libres, Rows1 y LtsF son de salida
partial_futoshiki(_,_,[],_,_,[]).
partial_futoshiki(_,[],_,_,_,[]).
partial_futoshiki(Rows, Lts, LPos, Rows1, Lts1, [Restr|LtsF]) :-
	not_unique_solution(Rows1, Lts1),
	set_cell(Rows, LPos, Rows1, LPos1),
	get_lessthans(Lts, LtsR, Restr),	
    partial_futoshiki(Rows, LtsR, LPos1, Rows1, [Restr|Lts1], LtsF).
partial_futoshiki(_, _, _, Rows1, Lts1, LtsF) :-
	unique_solution(Rows1, Lts1).

gen_futoshiki(N) :-
	gen_solution(N, Rows),
	gen_lessthans(N, Rows, Lts),
	gen_positions(N, LPos),
	empty_problem(N, Rows1),
	Lts1 = [],
	partial_futoshiki(Rows, Lts, LPos, Rows1, Lts1, LtsF),
	writeln('------Tablero------'),
	maplist(writeln, Rows1),
	writeln('---Restricciones---'),
	maplist(writeln, LtsF),!.
