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

e(L)  --> list(L).
e(I)  --> integer(I).

list(L) --> sequence("[",e,",","]",L).

packet((P1,P2))   --> list(P1),"\n",list(P2),"\n".

repack([])  --> [].
repack([(P1),(P2)|Ps])   --> [(P1,P2)],repack(Ps).

read13(Packets) :- phrase_from_file(sequence(packet,"\n",Packets),"d13.txt").
read13ex(Packets) :- phrase_from_file(sequence(packet,"\n",Packets),"ex13.txt").



compare((L,R),true) :- integer(L), integer(R), L #< R.
compare((L,R),false) :- integer(L), integer(R), L #> R.
compare((I,I),continue) :- integer(I).

compare(([],I),true) :- integer(I).
compare((I,[]),false) :- integer(I).
compare(([],[]),continue).

compare((L,[]),false) :- length(L,Len), Len #>= 1.
compare(([],R),true) :- length(R, Len), Len #>= 1.

compare(([L|_],[R|_]),true) :- compare((L,R),true).
compare(([L|_],[R|_]),false) :- compare((L,R),false).
compare(([L|Ls],[R|Rs]),Bool) :- compare((L,R),continue), compare((Ls,Rs),Bool).

compare((L,R),Bool) :- is_list(L), integer(R), compare((L,[R]),Bool).
compare((L,R),Bool) :- integer(L), is_list(R), compare(([L],R),Bool).

p1(Ans) :- read13(Ps), maplist(compare,Ps,T), zip_with_index(T,Ts), findall(X,member(X-true,Ts),Xs),sum_list(Xs,S), length(Xs,L), Ans #= S+L.

psort(<,E1,E2) :- compare((E1,E2),true).
psort(>,E1,E2) :- compare((E1,E2),false).
psort(=,E1,E2) :- compare((E1,E2),continue).

p2(Ans) :- 
    read13(P0), 
    phrase(repack(P1),P0), 
    append([([[2]]),([[6]])],P1,P2), 
    predsort(psort,P2,P3),
    nth1(I1,P3,[[2]]),
    nth1(I2,P3,[[6]]),
    Ans #= I1 * I2.

