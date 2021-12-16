% Exercise 11.1

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% member(3, [X, 3, Y]) -> [member(X, [X|_]), X = 3] -> X = 3
%                      |
%                      -> [member(X, [_|T]), X = 3, T = [3, Y]] member(3, [3, Y]) -> [member(X, [X|_]), X = 3] -> true
%                                                                                 |
%                                                                                 -> [member(X, [_|T]), X = 3, T = [Y]] member(3, [Y]) -> [member(X, [X|_]), X = Y = 3] -> Y = 3

% Exercise 11.2

prefix([], _).
prefix([X|Y], [X|Z]) :- prefix(Y, Z).

% Exercise 11.3

interleave([], [], []).
interleave([A1|A2], [], [A1|X]) :- interleave(A2, [], X).
interleave([], [B1|B2], [B1|X]) :- interleave([], B2, X).
interleave([A1|A2], [B1|B2], [A1|[B1|X]]) :- interleave(A2, B2, X).

% Exercise 11.4

squares([], []).
squares([A1|A2], [B1|B2]) :- B1 is A1 * A1, squares(A2, B2).

% Exercise 11.5

select([X|Y], X, Y).
select([X1|X2], C, [X1|Y]) :- select(X2, C, Y).

% Exercise 11.6

anagram([X], [X]).
anagram(X, [C|R1]) :- select(X, C, R), anagram(R, R1).

% Exercise 11.7

candidate(solution(Colors, Nationalities, Pets))
 :- anagram([red, blue, green], Colors),
    anagram([english, spanish, japanese], Nationalities),
    anagram([jaguar, snail, something], Pets).

person(solution([C|_], [N|_], [P|_]), person(C, N, P)).
person(solution([_|Colors], [_|Nats], [_|Pets]), Person)
	:- person(solution(Colors, Nats, Pets), Person).

condition1(solution([red|_], [english|_], _)).
condition1(solution([_|Colors], [_|Nats], [_|Pets]))
 :- condition1(solution(Colors, Nats, Pets)).

condition2(solution(_, [spanish|_], [jaguar|_])).
condition2(solution([_|Colors], [_|Nats], [_|Pets]))
 :- condition2(solution(Colors, Nats, Pets)).

condition3(solution(_, [_|[japanese|_]], [snail|_])).
condition3(solution([_|Colors], [_|Nats], [_|Pets]))
 :- condition3(solution(Colors, Nats, Pets)).

condition4(solution([_|[blue|_]], _, [snail|_])).
condition4(solution([_|Colors], [_|Nats], [_|Pets]))
 :- condition4(solution(Colors, Nats, Pets)).

solve(Solution)
 :- candidate(Solution),
    condition1(Solution),
    condition2(Solution),
    condition3(Solution),
    condition4(Solution).

% Exercise 11.8

sorted([_]).
sorted([X1|[X2|X]]) :- X1 < X2, sorted([X2|X]).

% Exercise 11.9

sort2(X, Y) :- anagram(X, Y), sorted(Y).
