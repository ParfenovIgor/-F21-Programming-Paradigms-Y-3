% Exercise 11.1

% 1
% nonunifiable(X, Y) :- \+ X = Y.

% 2
% nonunifiable(X, Y) :- X = Y, !, fail.
% nonunifiable(_, _).

% 3
nonunifiable(X, X) :- !, fail.
nonunifiable(_, _).

% Exercise 11.2(a)

% partition(_, [], [], []).
% partition(X, [A|B], [A|L], G) :- A < X, partition(X, B, L, G).
% partition(X, [A|B], L, [A|G]) :- A >= X, partition(X, B, L, G).

% Exercise 11.2(b)

partition(_, [], [], []).
partition(X, [A|B], [A|L], G) :- A < X, partition(X, B, L, G), !.
partition(X, [A|B], L, [A|G]) :- A >= X, partition(X, B, L, G), !.

% Exercise 11.3

parent(b, a).
parent(c, a).

sibling(A, B) :- \+ A = B, parent(A, C), parent(B, C).

% Exercise 11.4

connected(1,2).
connected(3,4).
connected(5,6). 
connected(7,8). 
connected(9,10). 
connected(12,13). 
connected(13,14). 
connected(15,16). 
connected(17,18). 
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11). 
connected(14,9). 
connected(11,15). 
connected(16,12). 
connected(14,17). 
connected(16,19).

path(A, A, [A]).
path(A, B, [A|D]) :- (connected(A, C); connected(C, A)), path(C, B, D), !.

% Exercise 11.5

filterUnifiable([], _, []).
filterUnifiable([A|B], T, [A|C]) :- \+ nonunifiable(A, T), filterUnifiable(B, T, C).
filterUnifiable([A|B], T, C) :- \+ A = T, filterUnifiable(B, T, C).

% Exercise 11.6

is_permutation([X|Y], Z) :- is_permutation(Y, W), remove_element(X, Z, W).   
is_permutation([], []).

remove_element(X, [X|R], R).
remove_element(X, [F|R], [F|S]) :- remove_element(X, R, S).

solve(P)
 :- is_permutation([1, 2, 3, 4, 5, 6, 7, 8], P), 
    build_diagonals([1, 2, 3, 4, 5, 6, 7, 8], P, S, D),
    different(S),
    different(D), !.

build_diagonals([], [], [], []).
build_diagonals([X1|X], [Y1|Y], [S1|S], [D1|D])
 :- S1 is X1 + Y1,
    D1 is X1 - Y1,
    build_diagonals(X, Y, S, D).

different([_]).
different([X|Y]) :- \+ member(X, Y), different(Y).
