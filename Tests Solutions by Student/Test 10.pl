student(alisa,  2).
student(bob,    1).
student(chloe,  2).
student(denise, 1).
student(edward, 2).

friend(alisa, bob).
friend(alisa, denise).
friend(bob, chloe).
friend(bob, edward).
friend(chloe, denise).
friend(denise, edward).

unary(z).
unary(s(X)) :- unary(X).

% 1

groupmates(X, Y) :- student(X, Z), student(Y, Z).

% 2

% friend(alisa, Y), friend(Y, Z) -> [friend(alisa, bob), Y = bob] friend(bob, Z) -> [friend(bob, chloe), Z = chloe] -> Y = bob, Z = chloe
%                                |                                               -> [friend(bob, edward), Z = edward] -> y = bob, Z = edward
%                                -> [friend(alisa, denise), Y = denise] friend(denise, Z) -> [friend(denise, edward), Z = edward] -> Y = denise, Z = edward

% 3

add(z, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

mult(z, _, z).
mult(s(X), Y, Z) :- add(Y, S, Z), mult(X, Y, S).
