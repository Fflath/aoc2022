:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(func)).

int_line(I) --> integer(I),"\n".
empty_line  --> "\n".
... --> [] | [_], ... .


input([])        --> call(eos), !.
input([C|Cs])    --> block(Cals),{sum_list(Cals,C)}, input(Cs).


block([C|Cs])   --> int_line(C),block(Cs).
block([])       --> empty_line | !.

eos([], []).

p1 :- 
    phrase_from_file(input(N), "d1.txt"), max_list(N,M),
    write(M).

p2 :-
    phrase_from_file(input(N), "d1.txt"),
    N2 = reverse $ sort $ N,
    length(A, 3),
    append(A, _, N2),
    sum_list(A,Sum), 
    write(Sum).

    