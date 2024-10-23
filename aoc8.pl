:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(assoc)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(utils).
:- use_module(dcg_utils).


move(_-[],[]).
move(X-[H|T],[X-H|T2]) :- move(X-T,T2).
adjust(X-(Y-Z),xyV(X,Y,Z)).


visible(List,Visible) :- 
    reverse(List,RList),
    phrase(visible_(V0),[-1|List]),
    phrase(visible_(V1),[-1|RList]),
    phrase(seqq([V0,V1]),V2),
    list_to_set(V2,Visible).
    
visible_(XYs) --> [TMax,xyV(_X,_Y,Z)], {Z #=< TMax}, push(TMax), visible_(XYs).
visible_([xyV(X,Y,Z)|XYs]) --> [TMax,xyV(X,Y,Z)], {Z #> TMax}, push(Z), visible_(XYs).
visible_([]) --> [_T].

p1(Count) :- 
    phrase_from_file(sequence(sequence(single_digit_int),"\n",M0),'d8.txt'),
    zip_with_index(M0,M1),
    maplist(move,M1,M2),
    maplist(zip_with_index,M2,M3),maplist(maplist(adjust),M3,Rows),
    transpose(Rows,Cols),
    append(Rows,Cols,RCs),
    maplist(visible,RCs,Vis),
    phrase(seqq(Vis),AVs), 
    list_to_set(AVs,S), 
    length(S,Count).


score(Rows,Cols,xyV(X,Y,V),Score) :- 
    nth0(X,Cols,ColX),
    nth0(Y,Rows,RowY),
    phrase(split(RB0,RA,xyV(X,Y,V)),RowY),
    phrase(split(CB0,CA,xyV(X,Y,V)),ColX),
    reverse(RB0,RB1),
    reverse(CB0,CB1),
    count_visible(V,RB1,0,S0),
    count_visible(V,CB1,0,S1),
    count_visible(V,RA,0,S2),
    count_visible(V,CA,0,S3),
    Score #=S0*S1*S2*S3.


split(Before,After,X) --> before(Before,X),after(After).
before([],X) --> [X].
before([H|T],X) --> [H], {dif(H,X)}, before(T,X).

after([H|T]) --> [H], after(T).
after([]) --> [].

count_visible(Rv,[xyV(_X,_Y,V)|T],Acc,End) :- 
    V #< Rv,
    AccN #= Acc + 1,
    count_visible(Rv,T,AccN,End).

count_visible(_Rv,[],End,End).
count_visible(Rv,[xyV(_X,_Y,V)|_T],Acc,End) :- 
    V #>= Rv,
    AccN #= Acc + 1,
    count_visible(Rv,[],AccN,End).

p2(Score) :- 
    phrase_from_file(sequence(sequence(single_digit_int),"\n",M0),'d8.txt'),
    zip_with_index(M0,M1),
    maplist(move,M1,M2),
    maplist(zip_with_index,M2,M3),maplist(maplist(adjust),M3,Rows),
    transpose(Rows,Cols),
    phrase(seqq(Rows),Map),
    maplist(score(Rows,Cols),Map, Scores),
    list_max(Scores,Score).
