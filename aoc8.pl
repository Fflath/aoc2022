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
:- use_module(library(list_util)).
:- use_module(library(lists)).

d(N) --> digit(D),{number_codes(N,[D])}.

move(_-[],[]).
move(X-[H|T],[X-H|T2]) :- move(X-T,T2).
adjust(X-(Y-Z),(X-Y)-Z).


map(In,M) :- M = maplist(adjust) $ flatten $ maplist(zip_with_index) $ maplist(move) $ zip_with_index $ In.

selector(D, Index, M, Res) :- tfilter(selector_(D,Index),M,Res).

selector_(row, Row, _C-Row-_H, true).
selector_(row, Row, _C-R-_H, false) :- dif(Row,R).
selector_(col, Col, Col-_R-_H, true).
selector_(col, Col, C-_R-_H, false) :- dif(Col,C).

count_visible([])                           --> [].
count_visible([H|T])                        --> count_visible_(H),count_visible(T).
count_visible_(P-H),[H,[P-H|Visible]]         --> [Tallest, Visible], {H #> Tallest}.
count_visible_(_P-H),[Tallest,Visible]      --> [Tallest, Visible], {H #=< Tallest}.

run_selector(_Sel,_M,[])        --> [].
run_selector(Sel,M,[I|Is])    --> run_selector_(Sel,I,M),run_selector(Sel,M,Is).
run_selector_(Sel,I,M),[Rn] --> [Rs], {
    selector(Sel,I,M,Selected),
    reverse(Selected, RSelected),
    phrase(count_visible(Selected),[-1,[]],[_,Vis0]),
    phrase(count_visible(RSelected),[-1,[]],[_,Vis1]),
    append(Vis0,Vis1,R),
    append(R,Rs,Rn)
    }.

p1(O) :- 
    phrase_from_file(sequence(sequence(d),"\n",Ds),"d8.txt"), 
    map(Ds,M), iota(99,Is),
    phrase(run_selector(row,M,Is),[[]],Rows),
    phrase(run_selector(col,M,Is),Rows,[All]),
    list_to_set(All,Alls),length(Alls,O).

map_location((C-R-H)) --> [C-R-H].

tree_selector(Col-Row-Height,Map,[Up,Down,Left,Right]) :- 
    selector(row,Row,Map,Cr),
    phrase(
        (sequence(map_location,L),
        map_location(Col-Row-Height),
        sequence(map_location,Right)),Cr),
    reverse(L,Left),
    selector(col,Col,Map,Cc),
    phrase(
        (sequence(map_location,U),
        map_location((Col-Row-Height)),
        sequence(map_location,Down)),Cc), 
    reverse(U,Up).

tree_score(Map, C-R-H, Score) :-
    tree_selector(C-R-H,Map,[U,D,L,Right]),
    phrase(count_scenic(H,U),[-1,0],[_,S0]),
    phrase(count_scenic(H,D),[-1,0],[_,S1]),
    phrase(count_scenic(H,L),[-1,0],[_,S2]),
    phrase(count_scenic(H,Right),[-1,0],[_,S3]),
    Score #= S0 * S1 * S2 * S3, !.


score_map(_Map,[])   --> [].
score_map(Map,[H|T])        -->  score_map_(Map, H), score_map(Map,T).

score_map_(Map, Tree),[High]       --> [High], {tree_score(Map, Tree, Score), High #> Score}.
score_map_(Map, Tree),[Score]      --> [High], {tree_score(Map, Tree, Score), High #=< Score}.

writer,[X]  --> [X], {write(X),nl}.


count_scenic(_H0,[])                           --> [].
count_scenic(H0,[H|T])                        --> count_scenic_(H0,H),count_scenic(H0,T).
count_scenic_(0,_),[T,0]                           --> [T,_].
count_scenic_(H0,_P-_H),[Tallest,Count]        --> [Tallest, Count], {Tallest #>= H0}.
count_scenic_(H0,_P-H),[H,Cn]                 --> [_, Count], {Tallest #< H0, H #> Tallest, Cn #= Count + 1}.
count_scenic_(H0,_P-H),[H,Count]                 --> [_, Count], {Tallest #< H0, H #=< Tallest}.

p2(X) :- phrase_from_file(sequence(sequence(d),"\n",Ds),"d8.txt"), map(Ds,M), phrase(score_map(M,M),[0],[X]).

% count_scenic_(H0,_P-H),[H,Cn]            --> [Tallest, Count], {H #=< H0, H #> Tallest, Cn #= Count + 1}.
% count_scenic_(H0,_P-H),[Tallest,Count]   --> [Tallest, Count], {H #=< H0, H #=< Tallest}.