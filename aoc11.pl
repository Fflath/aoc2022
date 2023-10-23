:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(func)).
:- use_module(reif).
:- use_module(library(dcg/high_order)).
:- use_module(library(assoc)).
:- use_module(library(apply)).
:- use_module(utils).
:- use_module(library(list_util)).
:- use_module(library(lists)).

plus(X,Y,Z) :- Z #= X + Y.
prod(X,Y,Z) :- Z #= X * Y.
prod(X,Z)   :- Z #= X * X.
divi(X,T,F,I,O) :- if_(divi_(X,I),O=T,O=F).
divi_(X,Y,true) :- 0 #= Y rem X.
divi_(X,Y,false) :- O #= Y rem X, dif(O,0).

remove_worry(W,W1) :- W1 #= W mod 9699690.



integer_list([])    --> [].
integer_list([I|Is])--> integer(I), ", ",integer_list(Is).

ml(ML) :- ML = [
    m(0,([72,97], prod(13), divi(19,5,6),0)),
    m(1,([55,70,90,74,95], prod, divi(7,5,0),0)),
    m(2,([74,97,66,57], plus(6), divi(17,1,0),0)),
    m(3,([86, 54, 53],plus(2),divi(13,1,2),0)),
    m(4,([50, 65, 78, 50, 62, 99],plus(3),divi(11,3,7),0)),
    m(5,([90],plus(4),divi(2,4,6),0)),
    m(6,([88, 92, 63, 94, 96, 82, 53, 53],plus(8),divi(5,4,7),0)),
    m(7,([70, 60, 71, 69, 77, 70, 98],prod(7),divi(3,2,3),0))].

monkey(m(N,M))  --> [m(N,M)].
monkeyList(MonkeyList)  --> sequence(monkey,MonkeyList).


update_monkey(M,U),[m(M,([U|Items],Op,Test,Count))]      --> get_monkey(m(M, (Items, Op, Test, Count))).

process_monkey(X) --> get_monkey(m(X,Monkey)), process_monkey_(m(X,Monkey)). 

process_monkey_(m(X,([],Op,Test,Count))),[m(X,([],Op,Test,Count))]    --> [].
process_monkey_(m(X,([Item|Items],Op,Test,Count))) --> {
    CountN #= Count + 1,
    Monkey = m(X,(Items,Op,Test,CountN)),
    call(Op, Item, W0), W1 #= W0 mod 9699690,%W1 #= W0 div 3,
    call(Test,W1,Dest)},
    update_monkey(Dest,W1),
    process_monkey_(Monkey).

get_monkey(m(N,M)),MonkeyListN --> 
    sequence(monkey,H),monkey(m(N,M)),sequence(monkey,T),
    {append(H,T,MonkeyListN)}.


round   --> round_([0,1,2,3,4,5,6,7]),{!}.
round_([])   --> [].
round_([M|Ms])   --> process_monkey(M),round_(Ms).

game(0)   --> [].
game(N)   --> round,{Rn #= N - 1},game(Rn).

score(X)    --> monkey(m(_,(_,_,_,X))).

p1(Score) :- 
    ml(ML),
    phrase(game(20),ML,X), 
    phrase(sequence(score,S0),X), 
    sort(S0,S1), reverse(S1,S2), nth1(1,S2,A), nth1(2,S2,B), Score #= A * B.

p2(Score) :-
ml(ML),phrase(game(10000),ML,X), phrase(sequence(score,S0),X), sort(S0,S1), reverse(S1,S2), nth1(1,S2,A), nth1(2,S2,B), Score #= A * B.