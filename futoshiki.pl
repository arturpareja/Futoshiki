:- use_module(library(clpfd)).
:- include('findnsols_lib.pl').

% medidor de tiempo
ponTiempo:- retractall(tiempo(_)), get_time(T),
        assert(tiempo(T)).

escribeTiempo:-
        get_time(T2), tiempo(T1), T is (T2-T1),
        nl, nl, write('Elapsed time: '),write(T), write(' secs.').

futoshiki(Rows, Lts) :-
        length(Rows, N), maplist(length_(N), Rows),
        append(Rows, Cells), Cells ins 1..N,        
        unequals(Rows, Lts),
        latin_square(Rows),
        label(Cells).

length_(L, Ls) :-
        length(Ls, L).

cell([I, J],Rows,C) :-    
        nth0(I,Rows,Row),
        nth0(J,Row,C).

%reset_cell([I, J], Rows, Rows1) :-    
%        nth0(I,Rows,Row),
%        nth0(J,Row,C),!,
%        select(Row,Rows,Row1, Rows1),   %sustituye Row por Row1 en Rows1
%        select(C,Row,_,Row1).           %sustituye el valor C por _ en Row1

unequal(Rows, [I1,J1,I2,J2]) :-
        cell([I1,J1], Rows, C1),
        cell([I2,J2], Rows, C2),
        C1 #< C2.

unequals(Rows, Lts) :- 
        maplist(unequal(Rows), Lts).

latin_square(Rows) :-
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns).

problem(1, 
        [[_,_,3,2,_],  % problem grid
         [_,_,_,_,_],
         [_,_,_,_,_],
         [_,_,_,_,_],
         [_,_,_,_,_]],
                        
        [[0,1,0,0],    % [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
         [0,3,0,4],
         [1,2,0,2],
         [2,2,1,2],
         [2,3,1,3],
         [1,4,2,4],
         [2,1,3,1],
         [3,3,3,2],
         [4,1,4,0],
         [4,3,4,2],
         [4,4,3,4]]).

problem(2, 
        [[_,_,_,2],  % problem grid
         [_,_,_,_],
         [_,_,_,_],
         [_,_,_,_]],
                        
        [[0,0,1,0],    % [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
         [0,1,1,1],
         [2,1,3,1],
         [2,3,3,3]]).

problem(3, 
        [[2,_,_],  % problem grid
         [_,_,_],
         [_,_,_]],
                        
        [[0,1,1,1]]).

solution(1,
         [[5,1,3,2,4],
          [1,4,2,5,3],
          [2,3,1,4,5],
          [3,5,4,1,2],
          [4,2,5,3,1]]).

solution(2,
         [[1, 3, 4, 2],
          [2, 4, 3, 1],
          [4, 1, 2, 3], 
          [3, 2, 1, 4]]).

futoshiki_solve(X, Rows) :- ponTiempo, problem(X, Rows, Lts), futoshiki(Rows, Lts), maplist(writeln, Rows), escribeTiempo.
futoshiki_check(X) :- futoshiki_solve(X, Rows1), solution(X, Rows2), Rows1 == Rows2.

unique_solution(Rows, Lts) :- 
    findnsols(2, Rows, futoshiki(Rows,Lts), L),
    length(L, 1).

not_unique_solution(Rows, Lts) :- 
    findnsols(2,Rows, futoshiki(Rows,Lts), L),
    length(L, N),
    N \= 1.
