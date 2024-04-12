:- use_module(library(dcgs)).
:- use_module(dcg_utils).   
:- use_module(library(pio)).

int_line(I) --> integer(I),"\n".
empty_line  --> "\n".

block(C)   --> sequence(int_line,Is), empty_line, {sum_list(Is,C)}.


p1 :- 
    phrase_from_file(sequence(block,N), "d1.txt"), 
    list_max(N,M),
    write(M).

p2 :-
    phrase_from_file(sequence(block,N), "d1.txt"),
    sort(N,N2), reverse(N2,N3),
    length(A, 3),
    append(A, _, N3),
    sum_list(A,Sum), 
    write(Sum).

    