:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(assoc)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(utils).
:- use_module(dcg_utils).
:- use_module(library(format)).


instr(I) --> instr_addx(I) | instr_noop(I).
instr_addx(addx(N)) --> "addx ", integer(N), "\n".
instr_noop(noop)    --> "noop\n".

run([]) --> [].
run([I|Is]) --> run_instr(I),run(Is).

run_instr(addx(N))    --> addx(N).
run_instr(noop)       --> noop.

addx(N),[CycleN,Xn,[Str|Signals],Times]        --> [Cycle,X,Signals,[Time|Times]], {
    sigcheck(2, Cycle, Time),
    Str #= Time * X,
    CycleN #= Cycle + 2, Xn #= X + N}.

addx(N),[CycleN,Xn,Signals,Times]        --> [Cycle,X,Signals,Times], {
    CycleN #= Cycle + 2, Xn #= X + N}.

noop,[CycleN,X,[Str|Signals],Times]         --> [Cycle,X,Signals,[Time|Times]], {
    sigcheck(1, Cycle, Time),
    CycleN #= Cycle + 1,
    Str #= Time * X
}. 

noop,[CycleN,X,Signals,Times]         --> [Cycle,X,Signals,Times], {CycleN #= Cycle + 1}. 

sigcheck(_, Cycle, Cycle).
sigcheck(N, Cycle, Signal) :- Signal #> Cycle, Signal #< Cycle + N.

p1(O) :- 
    phrase_from_file(sequence(instr,Is),"d10.txt"),
    phrase(run(Is),[1,1,[],[20,60,100,140,180,220]],[_,_,S,_]),sum_list(S,O).

run2([]) --> [].
run2([I|Is]) --> run2_instr(I),run2(Is).

run2_instr(addx(N))    --> screen(0),screen(1),addx(N).
run2_instr(noop)       --> screen(0),noop.

screen(Z),[Cycle,X,Signals,Times,["#"|Screen]]               --> [Cycle,X,Signals,Times,Screen], {
    Pixel #= (Cycle + Z - 1) mod 40,
    (X #= Pixel - 1; X #= Pixel ; X#= Pixel + 1)
    }.

screen(_),[Cycle,X,Signals,Times,["."|Screen]]               --> [Cycle,X,Signals,Times,Screen].

splitter([])    --> [].
splitter([(I)|Is]) --> {length(I0,40)},seq(I0),{reverse(I0,I)},splitter(Is).

printer([]).
printer([S|Ss]) :- phrase(seqq(S),S0), format("~s~n",[S0]),printer(Ss).

p2 :- 
    phrase_from_file(sequence(instr,Is),"d10.txt"),
    phrase(run2(Is),[1,1,[],[],[]],[_,_,_,_,X]),
    phrase(splitter(I),X),
    reverse(I,I0),
    printer(I0).

