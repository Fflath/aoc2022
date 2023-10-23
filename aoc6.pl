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

start_of_packet(B) --> {length(B,4)},string(B),{is_set(B)}.
start_of_message(M) --> {length(M,14)},string(M),{is_set(M)}.

p1(Out) :- phrase_from_file(string(In),"d6.txt"), phrase( (string(Head),start_of_packet(SOP)),In,_),append(Head,SOP,P),length(P,Out).
p2(Out) :- phrase_from_file(string(In),"d6.txt"), phrase( (string(Head),start_of_message(SOM)),In,_),append(Head,SOM,P),length(P,Out).