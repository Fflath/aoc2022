:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(func)).

lookup("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
priority(A,P) :- lookup(X), nth1(P,X,A).

lines([])        --> call(eos), !.
lines([S|Ss])   --> line(S),lines(Ss).
line(P)     --> string(Line), "\n", {
        length(R1,L), length(R2,L),
        append(R1,R2,Line),
        member(S,R1),
        member(S,R2),
        priority(S,P)
    }.
eos([], []).

p1 :- phrase_from_file(lines(In),"d3.txt"),sum_list(In,Out),write(Out).

lines2([]) --> call(eos), !.
lines2([S|Ss]) --> line2(S), lines2(Ss).
line2(P)    --> string(L1),"\n",string(L2),"\n",string(L3),"\n",
{
    member(I,L1),member(I,L2),member(I,L3),
    priority(I,P)
}.

p2 :- phrase_from_file(lines2(In), "d3.txt"), sum_list(In,Out), write(Out).