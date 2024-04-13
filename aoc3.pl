:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(dcg_utils).   
:- use_module(library(lists)).

lookup("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
priority(A,P) :- lookup(X), nth1(P,X,A).

lines(Ls)   --> sequence(line,Ls).
line(P)     --> string(Line), "\n", {
        length(R1,L), length(R2,L),
        append(R1,R2,Line),
        member(S,R1),
        member(S,R2),
        priority(S,P)
    }.

p1 :- phrase_from_file(lines(In),"d3.txt"),sum_list(In,Out),write(Out).


lines2(Ls)  --> sequence(line2,Ls).
line2(P)    --> string(L1),"\n",string(L2),"\n",string(L3),"\n",
{
    member(I,L1),member(I,L2),member(I,L3),
    priority(I,P)
}.

p2 :- phrase_from_file(lines2(In), "d3.txt"), sum_list(In,Out), write(Out).