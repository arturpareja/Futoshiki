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
	unequal(Rows, [I1,J1,I2,J2]),
	label(Vars).

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
partial_futoshiki(_,_,_,_,[],_,_,[]). %LPos vacia

partial_futoshiki(_,_,_,[],_,_,_,[]). %Lts vacia

partial_futoshiki(NC, NL, Rows, Lts, LPos, Rows1, Lts1, LtsF) :-
	NC > 0,
	set_cell(Rows, LPos, Rows1, LPos1),
	NC1 is NC -1,
    partial_futoshiki(NC1, NL, Rows, Lts, LPos1, Rows1, Lts1, LtsF).

partial_futoshiki(NC, NL, Rows, Lts, LPos, Rows1, Lts1, [Restr|LtsF]) :-
	NL > 0,
	get_lessthans(Lts, LtsR, Restr),
	NL1 is NL - 1,	
    partial_futoshiki(NC, NL1, Rows, LtsR, LPos, Rows1, [Restr|Lts1], LtsF).

%partial_futoshiki(0, 0, Rows, Lts, LPos, Rows1, Lts1, [Restr|LtsF]) :-
	%not_unique_solution(Rows1, Lts1),
	%set_cell(Rows, LPos, Rows1, LPos1),
	%get_lessthans(Lts, LtsR, Restr),	
    %partial_futoshiki(0, 0, Rows, LtsR, LPos1, Rows1, [Restr|Lts1], LtsF).

partial_futoshiki(0, 0, _, _, _, Rows1, Lts1, []) :-
	unique_solution(Rows1, Lts1).

gen_futoshiki(N) :-
	writeln('Dificultad? F: facil, D: dificil'),
	readln(Dif),
	gen_solution(N, Rows),
	%solution(2, Rows),
	gen_lessthans(N, Rows, Lts),
	gen_positions(N, LPos),
	empty_problem(N, Rows1),
	Lts1 = [], !,
	difficulty(N, Dif, NC, NL), 	
	partial_futoshiki(NC, NL, Rows, Lts, LPos, Rows1, Lts1, LtsF),!,
	%Escribir por pantalla
	writeln(' '),
	writeln('------ Tablero ------'),
	writeln(' '),
	maplist(printRows(N), Rows1),
	writeln(' '),
	writeln('--- Restricciones ---'),
	writeln(' '),
	maplist(printLts, LtsF),!.

printLts([X,Y,Z,W]) :- write([X,Y]), write(' < '), write([Z,W]), writeln('').

printRows(0, _) :- writeln(''), !.
printRows(N, [X|Xs]) :- var(X), !, write('-'), write('  '), N1 is N-1, printRows(N1, Xs).
printRows(N, [X|Xs]) :- write(X), write('  '), N1 is N-1, printRows(N1, Xs).

%Recibe el tamaño del tablero y un nivel de dificultad y devuelve NC y NL
difficulty(4, ['F'], 0, 7).
difficulty(4, ['F'], 1, 4).
difficulty(4, ['F'], 1, 5).
difficulty(4, ['F'], 1, 5).
difficulty(4, ['F'], 2, 4).
difficulty(4, ['F'], 3, 4).


difficulty(4, ['D'], 0, 4).
difficulty(4, ['D'], 0, 5).
difficulty(4, ['D'], 1, 4).
difficulty(4, ['D'], 2, 3).

difficulty(5, ['F'], 5, 11).
difficulty(5, ['D'], 1, 9).
%difficulty(5, ['D'], 4, 7). Funciona también
difficulty(6, ['F'], 7, 14).
difficulty(6, ['D'], 5, 11).



prueba_gen(N, NC, NL, Rows1, Lts1) :-
	gen_solution(N, Rows),
	gen_lessthans(N, Rows, Lts),
	gen_positions(N, LPos),
	empty_problem(N, Rows1),
	length(LPos, L1),
	length(Lts, L2),
	Len1 is L1 - 1,
	Len2 is L2 - 1,
	range(0, Len1, R1),
	range(0, Len2, R2),
	findall(Comb1,combination(NC,R1,Comb1), LComb1),
	findall(Comb2,combination(NL,R2,Comb2), LComb2),!,
	prueba(Rows, Lts, LPos, LComb1, LComb2, Rows1, Lts1),
	%Escribir por pantalla
	writeln(' '),
	writeln('------ Tablero ------'),
	writeln(' '),
	maplist(printRows(N), Rows1),
	writeln(' '),
	writeln('--- Restricciones ---'),
	writeln(' '),
	maplist(printLts, Lts1),!.

prueba(Rows, Lts, LPos, L1, L2, Rows1, Lts1) :-
	select(VPos, L1, _), %por cada combinacion de vectores de celdas
	select(VLts, L2, _), %por cada combinacion de vectores de restricciones
	set_cells(Rows, LPos, VPos, Rows1),
	set_restr(Lts, VLts, Lts1),
	unique_solution(Rows1, Lts1), !.


partial_futoshiki1(NC, NL, Rows, Lts, LPos, Rows1, Lts1) :-
	length(LPos, L1),
	length(Lts, L2),

	length(VPos, NC), %vamos a escoger tantas casillas como diga NC
	length(VLts, NL), %vamos a escoger tantas restricciones como diga NL

	Len1 is L1 - 1,
	Len2 is L2 - 1,
	range(0, Len1, R1),!,
	range(0, Len2, R2),!,

	VPos ins 0..Len1, %elementos de LPos escogidos
	VLts ins 0..Len2, %elementos de Lts escogidos

	all_different(VPos), %sin repeticion
	all_different(VLts), %sin repeticion

	combination(NC, R1, VPos),
	combination(NL, R2, VLts),

	set_cells(Rows, LPos, VPos, Rows1),
	set_restr(Lts, VLts, Lts1),
	unique_solution(Rows1, Lts1),

	label(VPos),
	label(VLts).


%Cada index de VPos tiene que ser distinto ==> all_different(VPos)
set_cells(_, _, [], _).
set_cells(Rows, LPos, [Index|VPos], Rows1) :-
	nth0(Index, LPos, P), %obtiene i-esima posicion P = [i,j] de LPos
	cell(P, Rows, C),	  %unifica la celda de Rows con la de Rows1
    cell(P, Rows1, C),
    set_cells(Rows, LPos, VPos, Rows1).  %llamada recursiva para unificar el resto de celdas de VPos

%Cada index de VPos tiene que ser distinto ==> all_different(VPos)
set_restr(_, [], []).
set_restr(Lts, [Index|VLts], [R|LtsR]) :-
	nth0(Index, Lts, R), %obtiene restriccion R = [i1,j1,i2,j2] de VLts
    set_restr(Lts, VLts, LtsR).  %llamada recursiva con el resto de VLts

gen_futoshiki1(N) :-
	writeln('Dificultad? F: facil, D: dificil'),
	readln(Dif),
	gen_solution(N, Rows),
	gen_lessthans(N, Rows, Lts),
	gen_positions(N, LPos),
	empty_problem(N, Rows1),
	difficulty(N, Dif, NC, NL), 	
	partial_futoshiki1(NC, NL, Rows, Lts, LPos, Rows1, Lts1) ,
	%Escribir por pantalla
	writeln(' '),
	writeln('------ Tablero ------'),
	writeln(' '),
	maplist(printRows(N), Rows1),
	writeln(' '),
	writeln('--- Restricciones ---'),
	writeln(' '),
	maplist(printLts, Lts1),!.

% combination(K,L,C) :- C is a list of K distinct elements
% chosen from the list L
combination(0,_,[]).
combination(K,L,[X|Xs]) :- 
	K > 0,
	el(X,L,R),
	K1 is K-1,
	combination(K1,R,Xs).

el(X,[X|L],L).
el(X,[_|L],R) :- 
	el(X,L,R).

% 1.22 (*):  Create a list containing all integers within a given range.

% range(I,K,L) :- I <= K, and L is the list containing all 
%    consecutive integers from I to K.
%    (integer,integer,list) (+,+,?)

range(I,I,[I]).
range(I,K,[I|L]) :- I < K, I1 is I + 1, range(I1,K,L).

%calcula el num de celdas y de restricciones de un critical set para tableros de tamaño N
%acorde a la formula 2*NC + 3*NL >= 2*(N-1)
%ej. para tam 3 genera: (2,0),(1,1),(0,2)
critical_set_size(3, NC, NL) :-
	NC is random(3), %0..2
	NL is 2 - NC.

critical_set_size(4, NC, NL) :-
	NC is random(5), %0..4
	NL is 4 - NC.

critical_set_size(N, NC, NL) :-
	N > 4,
	N1 is N + 1,
	N2 is N1 + 1,
	NC is random(N2), %0..N+1
	NL is N1 - NC.


% IDEA 2: usar findnsols para obtener una lista con varios LPos y elegir uno con random_member para que sea mas aleatorio
% IDEA 3: realizar recursion sobre NC y NL, obteniendo una pos [i,j] de LPos con random_member y generar directamente Rows1 y Lts1

%debe proporcionar informacion sobre al menos n-1 filas y n-1 columnas
%devuelve la lista de coordenadas (i,j) LPos en las que se colocarán las celdas y restricciones
%en dichas celdas los numeros tienen que ser distintos
critical_set(N, Rows, LPos) :-
	N1 is N - 1,
	length(LX, N1), %lista de tamaño N-1
	length(LY, N1), %lista de tamaño N-1
	LX ins 0..N1,   %dominio de 0 a N-1, es decir, N posibles elementos => una fila no se escoge
	LY ins 0..N1,   %dominio de 0 a N-1, es decir, N posibles elementos => una columna no se escoge
	all_different(LX),
	all_different(LY),	
	merge_lists(LX,LY,LPos),
	get_cells(LPos, Rows, Cells),
	all_different(Cells),
	label(LX),
	label(LY).

%combina los elementos de 2 listas en una lista de pares
%solo funciona con listas del mismo tamaño
merge_lists([], [], []).
merge_lists([A|As], [B|Bs], [[A,B]|Rs]) :-
    !, merge_lists(As, Bs, Rs).

%obtiene el valor de cada una de las celdas indicadas en LPos
get_cells([], _, []).
get_cells([P|LPos], Rows, [C|Cells]) :-
	cell(P, Rows, C),
	get_cells(LPos, Rows, Cells).

% (NC,NL) = critical_set_size
% Rows = gen_solution
% Lts = gen_lessthans
% LPos = critical_set
% Rows1 = empty_problem
% Lts1 = length(Lts1, NL)
% LPos contiene la posicion de NC celdas y de NL restricciones
% La recursion se acaba cuando LPos se vacia => NC y NL = 0
critical_futoshiki(0, 0, _, _, [], _, _). 
critical_futoshiki(NC, NL, Rows, Lts, LPos, Rows1, Lts1) :-
	NC > 0,
	NC1 is NC - 1,
	set_cell(Rows, LPos, Rows1, LPos1),
	critical_futoshiki(NC1, NL, Rows, Lts, LPos1, Rows1, Lts1).

critical_futoshiki(NC, NL, Rows, Lts, LPos, Rows1, Lts1) :-
	NL > 0,
	NL1 is NL - 1,
	set_lts(NL1, Lts, LPos, Lts1, LPos1),
	critical_futoshiki(NC, NL1, Rows, Lts, LPos1, Rows1, Lts1).

% IDEA 4: meter un random en la primera clausula 
% para que escoja I1, J1 o I2, J2 con la misma probabilidad
% Index 0..N-1
%set_lts(Index, Lts, LPos, Lts1, LPos1) :-
%	random(2) > 0,
%	random_member([I,J], LPos), %Obtiene P = [i,j] aleatoriamente de LPos
%	delete(LPos, [I,J], LPos1),
%	Elem = [I,J,_,_],
%	member(Elem,Lts),			%Busca un elemento de Lts con esas coordenadas en I1,J1
%	nth0(Index, Lts1, Elem). 	%Lo unifica en la i-esima posicion de Lts1

% IMPORTANTE: la coordenada que escojamos de LPos, 
% siempre va a indicar las coordenadas I2, J2 de la coordenada (I1,J1) < (I2, J2)
% en cualquier otro caso, la construccion del conjunto critico esta mal 
set_lts(Index, Lts, LPos, Lts1, LPos1) :-
	random_member([I,J], LPos), %Obtiene P = [i,j] aleatoriamente de LPos
	delete(LPos, [I,J], LPos1),
	Elem = [_,_,I,J],
	member(Elem,Lts),			%Busca un elemento de Lts con esas coordenadas en I2, J2
	nth0(Index, Lts1, Elem). 	%Lo unifica en la i-esima posicion de Lts1

generate_futoshiki(N) :-
	gen_solution(N, Rows),
	gen_lessthans(N, Rows, Lts),
	empty_problem(N, Rows1),
	critical_set_size(N, NC, NL), 
	critical_set(N, Rows, LPos),
	length(Lts1, NL),
	critical_futoshiki(NC, NL, Rows, Lts, LPos, Rows1, Lts1),
	print_futoshiki(N, Rows1, Lts1),!,
	unique_solution(Rows1,Lts1).

print_futoshiki(N, Rows1, Lts1) :-
%Escribir por pantalla
	writeln(' '),
	writeln('------ Tablero ------'),
	writeln(' '),
	maplist(printRows(N), Rows1),
	writeln(' '),
	writeln('--- Restricciones ---'),
	writeln(' '),
	maplist(printLts, Lts1).



not_in([], _, _).
not_in([[X1,Y1]|LPos], X, Y) :-
	X1 #\= X,
	Y1 #\= Y,
	not_in(LPos, X, Y).

%Se dejan libres la fila X y la columna Y
critical(N, H, X, Y, LPos) :-
	N1 is N - 1,
	length(Xs, H),
	length(Ys, H),
	X is random(N), % fila descartada
	Y is random(N), % columna descartada
	Xs ins 0..N1, 	% las X entre 0..N -1
	Ys ins 0..N1, 	% las Y entre 0..N -1
	merge_lists(Xs, Ys, LPos), 	% genera lista de pares [Xi, Yi]
	not_in(LPos, X, Y),			% comprueba que Xi, Yi sean distintos de X e Y descartados
	%=====ALL DISTINCT PARA LISTAS =======
	maplist(decimal_hashing, LPos, LHash), 	% obtenemos el hash de cada [X,Y]
	all_distinct(LHash),					% comprobamos que todos los hashes sean distintos
	%=====================================
	label(Xs),
	label(Ys).

decimal_hashing([X,Y], Z) :-
	Z #= X * 10 + Y.