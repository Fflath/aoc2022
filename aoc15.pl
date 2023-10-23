:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(reif).
:- use_module(library(dcg/high_order)).
:- use_module(library(assoc)).
:- use_module(library(apply)).
:- use_module(utils).
:- use_module(library(list_util)).
:- use_module(library(lists)).
:- use_module(library(assoc)).

point((X,Y))    --> "x=", integer(X), ", y=",integer(Y).
line((S-B))     --> "Sensor at ",point(S),": closest beacon is at ",point(B),"\n".
file(Ls)    --> sequence(line,Ls).

distance((X1,Y1)-(X2,Y2),Distance) :- utils:absF(X1 - X2, Dx), utils:absF(Y1 - Y2, Dy), Distance #= Dx + Dy.

read_file(File,SBs) :- phrase_from_file(file(SBs),File).


transform(S-B, S-B-Distance) :- distance(S-B,Distance).

is_possible(P, S-B-Distance,true) :- dif(B,P),dif(P,S),distance(S-P,D2), Distance #< D2.
is_possible(P, S-B-Distance,false) :- dif(B,P),dif(P,S),distance(S-P,D2), Distance #>= D2.
is_possible(S, S-B-Distance,true).
is_possible(B, S-B-Distance,true).

all_possible(SensorDistances,Point,Bool) :- maplist(is_possible(Point),SensorDistances,Ps), fold_left(bool_and,Ps,true,Bool),!.

make_point(Y,X,(X,Y)).

p1(A) :- read_file("d15.txt",SBs),maplist(transform,SBs,SDs), phrase(p1_counter(SDs,2000000,-2000000,5000000),[0],[A]).


p1_counter(_SensorDistances,_Y,MaxX,MaxX)            --> [].
p1_counter(SensorDistances,Y,X,MaxX)        --> p1_counter_(SensorDistances,Y,X),{Xn#=X+1},p1_counter(SensorDistances,Y,Xn,MaxX),!.

p1_counter_(SensorDistances,Y,X), [CountN]  --> [Count], {all_possible(SensorDistances,(X,Y),false), CountN #= Count + 1}.
p1_counter_(SensorDistances,Y,X)            --> {all_possible(SensorDistances,(X,Y),true)}.


constraint(S-B,A) :- distance(S-B,D1), distance(S-A,D2), D1 #< D2.

p2(A) :-
    AX in 0..4000000, AY in 0..4000000,
    read_file("d15.txt",SBs),
    phrase(constraint(SBs),[(AX,AY)],[A]).


constraint([])   --> [].
constraint([H|T])--> constraint_(H),constraint(T).
constraint_(H),[B]--> [B], {constraint__(H,B)}.
constraint__(S-B,A) :- distance(S-B,D1), D1 #< D2, distance(S-A,D2).


pair_intersection((SB1,SB2),Points) :- ring(SB1,P1), ring(SB2,P2), intersection(P1,P2,Points).
ring(S-B,Points) :- distance(S-B, D1), D2 #= D1+1, findall(P, distance(S-P,D2),Points).
