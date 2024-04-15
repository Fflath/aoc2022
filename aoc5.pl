:- use_module(library(dcgs)).
:- use_module(dcg_utils).   
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(assoc)).
:- use_module(utils).
:- use_module(library(lists)).
:- use_module(library(dif)).

stack_op(S,V,[V|S]).

move(Src,Dst,SrcN,DstN) :- 
    stack_op(SrcN,V,Src),
    stack_op(Dst,V,DstN).

state_change((0,_,_),State,State).
state_change((Count,Src,Dst),State,StateN) :-
    get_assoc(Src, State, S), get_assoc(Dst,State,D),
    move(S,D,Sn,Dn),
    put_assoc(Src,State,Sn,State1),put_assoc(Dst,State1,Dn,State2),
    Cn #= Count - 1,
    state_change((Cn,Src,Dst),State2,StateN).

state_change9001((Count,Src,Dst),State,StateN) :-
    get_assoc(Src, State, S), get_assoc(Dst,State,D),
    length(SH,Count), append(SH,Sn,S), append(SH,D,Dn),
    put_assoc(Src,State,Sn,State1),put_assoc(Dst,State1,Dn,StateN).

crate(C)     --> "[",string(C),"]".
crate(nil)   --> "   ".
row(Cs)     --> sequence(crate," ",Cs).
map(M)      --> sequence(row,"\n",M).

not_nil(nil,false).
not_nil(X,true) :- dif(X,nil).


weave([[],[],[],[],[],[],[],[]],[]).
weave([[H1|T1],[H2|T2],[H3|T3],[H4|T4],[H5|T5],[H6|T6],[H7|T7],[H8|T8]],[[H1,H2,H3,H4,H5,H6,H7,H8]|T9]) :- 
    weave([T1,T2,T3,T4,T5,T6,T7,T8],T9).

instruction((Count,Src,Dst)) --> "move ",integer(Count)," from ", integer(Src), " to ", integer(Dst), "\n". 
instructions(Is)    --> sequence(instruction,Is).

run_instruction(Instruction),[StateN]    --> [State],{state_change(Instruction,State,StateN)}.
run_instructions([]) --> [].
run_instructions([I|Is]) --> run_instruction(I),run_instructions(Is).

run9001_instruction(Instruction),[StateN]    --> [State],{state_change9001(Instruction,State,StateN)}.
run9001_instructions([]) --> [].
run9001_instructions([I|Is]) --> run9001_instruction(I),run9001_instructions(Is).

p1(Out) :- 
    phrase_from_file((map(M),...,instructions(Is)),"d5.txt"),
    weave(M,M1),
    maplist(tfilter(not_nil),M1, SN),
    zip([1,2,3,4,5,6,7,8,9],SN,O),
    list_to_assoc(O,State0),
    phrase(run_instructions(Is),[State0],[State1]),
    assoc_to_values(State1,State2),
    maplist(nth1(1),State2, State3),
    phrase(seqq(State3),Out).

p2(Out) :- 
    phrase_from_file((map(M),...,instructions(Is)),"d5.txt"),
    weave(M,M1),
    maplist(tfilter(not_nil),M1, SN),
    zip([1,2,3,4,5,6,7,8,9],SN,O),
    list_to_assoc(O,State0),
    phrase(run9001_instructions(Is),[State0],[State1]),
    assoc_to_values(State1,State2),
    maplist(nth1(1),State2, State3),
    phrase(seqq(State3),Out).
