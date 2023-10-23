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

crate(C) --> "[",string(C),"]".
crate(nil)   --> "   ".

not_nil(nil,false).
not_nil(X,true) :- dif(X,nil).

zip([],[],[],[],[],[],[],[],[],[]).
zip([H1|T1],[H2|T2],[H3|T3],[H4|T4],[H5|T5],[H6|T6],[H7|T7],[H8|T8],[H9|T9],[H1,H2,H3,H4,H5,H6,H7,H8,H9|T10]) :- 
    zip(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10).

instruction((Count,Src,Dst)) --> "move ",integer(Count)," from ", integer(Src), " to ", integer(Dst), "\n". 


run_instruction(Instruction),[StateN]    --> [State],{state_change(Instruction,State,StateN)}.
run_instructions([]) --> [].
run_instructions([I|Is]) --> run_instruction(I),run_instructions(Is).

run9001_instruction(Instruction),[StateN]    --> [State],{state_change9001(Instruction,State,StateN)}.
run9001_instructions([]) --> [].
run9001_instructions([I|Is]) --> run9001_instruction(I),run9001_instructions(Is).

p1(Out) :- 
    phrase_from_file((sequence(sequence(crate," "),"\n",M),...,sequence(instruction, Is)),"d5.txt"),
    flatten(M,M1),zip(S1,S2,S3,S4,S5,S6,S7,S8,S9,M1), 
    maplist(tfilter(not_nil),[S1,S2,S3,S4,S5,S6,S7,S8,S9], SN),
    zip([1,2,3,4,5,6,7,8,9],SN,O),
    list_to_assoc(O,State0),
    phrase(run_instructions(Is),[State0],[State1]),
    assoc_to_values(State1,State2),
    maplist(nth1(1),State2, State3),
    maplist(swap(char_code),State3,Out).

p2(Out) :- 
    phrase_from_file((sequence(sequence(crate," "),"\n",M),...,sequence(instruction, Is)),"d5.txt"),
    flatten(M,M1),zip(S1,S2,S3,S4,S5,S6,S7,S8,S9,M1), 
    maplist(tfilter(not_nil),[S1,S2,S3,S4,S5,S6,S7,S8,S9], SN),
    zip([1,2,3,4,5,6,7,8,9],SN,O),
    list_to_assoc(O,State0),
    phrase(run9001_instructions(Is),[State0],[State1]),
    assoc_to_values(State1,State2),
    maplist(nth1(1),State2, State3),
    maplist(swap(char_code),State3,Out).
