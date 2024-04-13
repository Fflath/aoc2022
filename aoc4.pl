:- use_module(library(dcgs)).
:- use_module(dcg_utils).   
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(lists)).

between(X,Y,Z,true) :- Z #>= X, Z #=< Y.
between(X,_Y,Z,false) :- Z #=< X.
between(_X,Y,Z,false) :- Z #>= Y.
all_in((A-B,X-Y),true) :- between(A,B,X,true), between(A,B,Y,true).
all_in((A-B,X-Y),true) :- between(X,Y,A,true), between(X,Y,B,true).
all_in((A-B,X-Y),false):- between(A,B,X,true), between(A,B,Y,false).
all_in((A-B,X-Y),false):- between(A,B,X,false), between(A,B,Y,true).
all_in((A-B,X-Y),false):- between(X,Y,A,true), between(X,Y,B,false).
all_in((A-B,X-Y),false):- between(X,Y,A,false), between(X,Y,B,true).
all_in((A-B,X-Y),false):- between(A,B,X,false), between(A,B,Y,false).
all_in((A-B,X-Y),false):- between(X,Y,A,false), between(X,Y,B,false).

overlap(R,true):- all_in(R,true).
overlap((A-B,X-Y),true):- between(A,B,X,true), between(A,B,Y,false).
overlap((A-B,X-Y),true):- between(A,B,X,false), between(A,B,Y,true).
overlap((A-B,X-Y),true):- between(X,Y,A,true), between(X,Y,B,false).
overlap((A-B,X-Y),true):- between(X,Y,A,false), between(X,Y,B,true).
overlap((A-B,X-Y),false):- between(A,B,X,false), between(A,B,Y,false).
overlap((A-B,X-Y),false):- between(X,Y,A,false), between(X,Y,B,false).



line((A-B,X-Y)) --> integer(A),"-",integer(B),",",integer(X),"-",integer(Y),"\n".
lines(Ss)   --> sequence(line,Ss).

p1 :-   phrase_from_file(lines(In),"d4.txt"),
        tfilter(all_in,In,Out),
        length(Out, X), 
        write(X).

p2 :-  phrase_from_file(lines(In),"d4.txt"),tfilter(overlap,In,Out),length(Out, X), write(X).