% Exercise 10.1

house_elf(dobby). 
witch(hermione). 
witch('McGonagall'). 
witch(rita_skeeter). 
magic(X) :- house_elf(X). 
%magic(X) :- wizard(X). 
magic(X) :- witch(X).

% magic (X = hermione) = or (house_elf (hermione), wizard (hermione), witch (hermione)) = or (wizard (hermione), witch (hermione)) = or (witch (hermione)) = or (true) = true

% Exercise 10.2

word(astante, a,s,t,a,n,t,e). 
word(astoria, a,s,t,o,r,i,a). 
word(baratto, b,a,r,a,t,t,o). 
word(cobalto, c,o,b,a,l,t,o). 
word(pistola, p,i,s,t,o,l,a). 
word(statale, s,t,a,t,a,l,e).

crossword(H1, H2, H3, V1, V2, V3) :-
	word(H1, _, A, _, B, _, C, _),
	word(H2, _, D, _, E, _, F, _),
	word(H3, _, G, _, H, _, I, _),
	word(V1, _, A, _, D, _, G, _),
	word(V2, _, B, _, E, _, H, _),
	word(V3, _, C, _, F, _, I, _),
    H1 \= V1, H2 \= V2, H3 \= V3.
    
% Exercise 10.3

directTrain(saarbruecken,dudweiler). 
directTrain(forbach,saarbruecken). 
directTrain(freyming,forbach). 
directTrain(stAvold,freyming). 
directTrain(fahlquemont,stAvold). 
directTrain(metz,fahlquemont). 
directTrain(nancy,metz).

travelFromTo(X, Y) :- directTrain(X, Y).
travelFromTo(X, Y) :- directTrain(X, Z), travelFromTo(Z, Y).

% Exercise 10.4

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

path(X, X).
path(X, Y) :- connected(X, Y).
path(X, Y) :- connected(X, Z), path(Z, Y).

% Exercise 10.5

byCar(auckland,hamilton). 
byCar(hamilton,raglan). 
byCar(valmont,saarbruecken). 
byCar(valmont,metz). 

byTrain(metz,frankfurt). 
byTrain(saarbruecken,frankfurt). 
byTrain(metz,paris). 
byTrain(saarbruecken,paris). 

byPlane(frankfurt,bangkok). 
byPlane(frankfurt,singapore). 
byPlane(paris,losAngeles). 
byPlane(bangkok,auckland). 
byPlane(singapore,auckland). 
byPlane(losAngeles,auckland).

travel(X, Y) :- byCar(X, Y).
travel(X, Y) :- byTrain(X, Y).
travel(X, Y) :- byPlane(X, Y).
travel(X, Y) :- byCar(X, Z), travel(Z, Y).
travel(X, Y) :- byTrain(X, Z), travel(Z, Y).
travel(X, Y) :- byPlane(X, Z), travel(Z, Y).

% Exercise 10.6

travel(X, Y, Z) :- byCar(X, Y), Z = goByCar(X, Y).
travel(X, Y, Z) :- byTrain(X, Y), Z = goByTrain(X, Y).
travel(X, Y, Z) :- byPlane(X, Y), Z = goByPlane(X, Y).
travel(X, Y, Z) :- byCar(X, W), travel(W, Y, Z_), Z = goByCar(X, W, Z_).
travel(X, Y, Z) :- byTrain(X, W), travel(W, Y, Z_), Z = goByTrain(X, W, Z_).
travel(X, Y, Z) :- byPlane(X, W), travel(W, Y, Z_), Z = goByPlane(X, W, Z_).
