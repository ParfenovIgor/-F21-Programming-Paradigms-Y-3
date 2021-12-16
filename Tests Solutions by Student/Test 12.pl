% 1

min(A, B, A) :- A < B.
min(A, B, B) :- A >= B.

minimum([A], A).
minimum([A|[B|C]], M) :- minimum([B|C], M1), min(A, M1, M).

% 2

remove(_, [], []).
remove(A, [A|B], C) :- remove(A, B, C).
remove(A, [B|C], [B|D]) :- A \= B, remove(A, C, D).

% 3

nonunifiable(X, Y) :- \+ X = Y.

removeU(_, [], []).
removeU(T, [A|B], C) :- \+ nonunifiable(A, T), removeU(T, B, C).
removeU(T, [A|B], [A|C]) :- \+ A = T, removeU(T, B, C).

% 4

nat(0).
nat(N) :- nat(K), N is K+1.

nat(0, 0) :- !.
nat(0, Max) :- Max > 0.
nat(N, Max) :- M is Max-1 , nat(K, M), N is K+1.

composite(N) :-
    nat(X, N), X > 1, X < N,
    nat(Y, N), Y > 1, Y < N,
    N is X*Y, !.

prime(N) :-
    nat(N), N > 1, \+ composite(N).

% 5

color(red).
color(green).
color(blue).

distinctColors([]).
distinctColors([H|T]) :-
    distinctColors(T),
    color(H),
    \+ member(H, T).

allColors(L) :-
    distinctColors(L),
    \+ (color(C), \+ member(C, L)).
