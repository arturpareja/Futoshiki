:- use_module(library(clpfd)).

%genera cuadrados latinos de dimension N, lo devuelve en Rows
gen_latinsq(N, Rows) :-
        length(Rows, N), maplist(length_(N), Rows),
        append(Rows, Cells), Cells ins 1..N,
        latin_square(Rows),
        label(Cells).		

%comprueba la restriccion de que sea cuadrado latino
latin_square(Rows) :-
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns).

%length auxiliar 
length_(L, Ls) :-
        length(Ls, L).

%genera las restricciones de una sola fila
%recibe la fila, numfila, columna0, columna1
restr([_], _, _, _, []).
restr([X,Y|Ys], I1, J1, J2, [[I1,J1,I1,J2]| Ls]) :- 
		X #< Y,
		J3 is J2 + 1,
		restr([Y|Ys], I1, J2, J3, Ls).

restr([X,Y|Ys], I1, J1, J2, [[I1,J2,I1,J1]|Ls]) :- 	
		X #> Y,
		J3 is J2 + 1,
		restr([Y|Ys], I1, J2, J3, Ls).

%genera las restricciones para todas las filas
%recibe la dimension N, la fila actual y la matriz Rows
map_restr(N, N, _, []) .
map_restr(N, I, Rows, [Ls|Lts]) :- 
		nth0(I,Rows,Row),
		restr(Row,I,0,1,Ls),
		I1 is I + 1,
		map_restr(N, I1, Rows, Lts).

%aplana lo que devuelve map_restr
nested_restr(N, I, Rows, Lts):-
	map_restr(N, I, Rows, Ls),
	append(Ls, Lts).

%nested_restr(2,0,[[5,1,3,2,4],[1,4,2,5,3]], Lts).

%cambia el elemento en Pos, por una variable anonima
reset_cell(Pos, Row, Rst_Row) :-
	nth0(Pos,Row,E),
	delete(Row,E,Ls),
	nth0(Pos,Rst_Row,_,Ls).

%cambia la posicion i,j de la matriz por una variable anonima
reset_row(I,J,Rows, Rows1) :- 
	nth0(I,Rows, Row),
	reset_cell(J, Row, Rst_Row),
	delete(Rows, Row, Rst_Rows),
	nth0(I, Rows1, Rst_Row, Rst_Rows).

%genera la lista de pares [i,j] de la matriz, de dimension N

%gen_positions(N, LPos) :- 
%	gen_positions1(N, 0, 0, LPos).


	






