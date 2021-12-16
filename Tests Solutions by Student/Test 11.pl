% Exercise 1

sublist([A|B], [A|C]) :- sublist(B, C).
sublist([A|B], [_|C]) :- sublist([A|B], C).
sublist([], _).

% Exercise 2

pref([], _).
pref([A|B], [A|C]) :- pref(B, C).

search(A, B, 0) :- pref(A, B).
search(A, [_|C], Pos) :- search(A, C, Pos2), Pos is Pos2 + 1.

% Exercise 3

cut(A, 0, A).
cut([_|B], N, C) :- N1 is N - 1, cut(B, N1, C).

append([], A, A).
append([A|B], D, [A|C]):- append(B, D, C).

replace(_, _, A, A).
replace(A, B, [C|D], [C|E]) :- replace(A, B, D, E).
replace(A, C, B, D) :- pref(A, B), length(B, L), cut(D, L, D1), append(D1, C, D2), replace(A, C, B, D2).
