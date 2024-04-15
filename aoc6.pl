:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(dcg_utils).
:- use_module(utils).

start_of_packet(B) --> {length(B,4)},string(B),{
    list_to_set(B,B)}.
start_of_message(M) --> {length(M,14)},string(M),{list_to_set(M,M)}.

p1(Out) :- 
    phrase_from_file((string(In),"\n"),"d6.txt"), 
    phrase( (read(Head),start_of_packet(SOP)),In,_),
    append(Head,SOP,P),
    length(P,Out).

p2(Out) :- 
    phrase_from_file((string(In),"\n"),"d6.txt"), 
    phrase( (read(Head),start_of_message(SOM)),In,_),
    append(Head,SOM,P),
    length(P,Out).