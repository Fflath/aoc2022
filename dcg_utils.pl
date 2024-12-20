:- module(dcg_utils,
    [
        alpha_lower//1,
        alpha_upper//1,
        alpha//1,
        digit//1,
        digits//1,
        integer//1,
        alpha_numeric//1,
        white_space//0,
        sequence//2,
        sequence//3,
        wrap//4,
        list//1,
        string//1,
        read//1,
        single_digit_int//1,
        push//1
    ]).

:- use_module(library(lists)).
:- use_module(library(serialization/abnf)).
:- use_module(library(dcgs)).
:- use_module(library(clpz)).

:- meta_predicate sequence(1,?,?,?).
:- meta_predicate sequence(1,?,?,?,?).

alpha_lower(C)  --> [C], {member(C,"abcdefghijklmnopqrstuvwxyz")}.
alpha_upper(C)  --> [C], {member(C,"ABCDEFGHIJKLMNOPQRSTUVWXYZ")}.
alpha(C)        --> alpha_lower(C) | alpha_upper(C).
digit(C)        --> [C], {member(C,"0123456789")}.
white_space     --> [C], {member(C, " \t")}.


sequence(Parser, List)          --> sequence_(List, Parser).
sequence_([H|T], Parser)        --> call(Parser, H), sequence_(T, Parser).
sequence_([], _)                --> [].

sequence(Parser, Sep, List)     --> sequence_(List, Parser, Sep).
sequence_([H|T], Parser, Sep)   --> call(Parser, H), Sep, sequence_(T, Parser, Sep).
sequence_([H], Parser, _Sep)    --> call(Parser, H).

wrap(Start, End, Parser, List)  --> [Start],call(Parser, List),[End].
list(L)                         --> "[",sequence(alpha_numeric,',',L),"]".

digits([D|Ds])          --> digit(D),sequence(digit,Ds).
integer(I)          --> digits(Ds),{number_chars(I,Ds)}.
integer(I)          --> "-",digits(Ds),{number_chars(I0,Ds),I #= -1 * I0}.
alpha_numeric(C)    --> alpha_lower(C) | alpha_upper(C) | digit(C).
string(S)           --> sequence(alpha_numeric,S).

% lazy read
read([])    --> [].
read([H|T]) --> [H], read(T).

single_digit_int(0) --> "0".
single_digit_int(1) --> "1".
single_digit_int(2) --> "2".
single_digit_int(3) --> "3".
single_digit_int(4) --> "4".
single_digit_int(5) --> "5".
single_digit_int(6) --> "6".
single_digit_int(7) --> "7".
single_digit_int(8) --> "8".
single_digit_int(9) --> "9".

push(X), [X] --> [].