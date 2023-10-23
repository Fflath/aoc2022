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
:- use_module(library(assoc)).

point((X,Y))    --> integer(X),",",integer(Y).
path(P)         --> sequence(point," -> ",P),"\n".
paths(Ps)       --> sequence(path,Ps).

read_ex14(Paths) :- phrase_from_file(paths(Paths),"ex14.txt").

read_14(Paths) :- phrase_from_file(paths(Paths),"d14.txt").

starting_state(Grid) :- 
    read_14(Paths), 
    phrase(grid_it_up(Paths),[],Grid0), 
    list_to_set(Grid0,Grid1), 
    list_to_assoc(Grid1,Grid).

clean :- retractall(grid(_,_)).


grid_it_up([]) --> [].
grid_it_up([Path|Ps]) --> grid_it_up_(Path), grid_it_up(Ps).
grid_it_up_([_])    --> [].
grid_it_up_([P1,P2|Ps]) --> fillin_(P1,P2),grid_it_up_([P2|Ps]).

fillin_(P1,P2),Pn  --> {fillin(P1,P2,[],Pn)}.

fillin(P,P,Ps,[P-rock|Ps]).
fillin((PX,P1Y),(PX,P2Y),Ps,Pn) :- P1Y #> P2Y, fillin((PX,P2Y),(PX,P1Y),Ps,Pn).
fillin((PX,P1Y),(PX,P2Y),Ps,Pn) :- P1Y #< P2Y, 
    PYn #= P1Y+1,
    fillin((PX,PYn),(PX,P2Y),[(PX,P1Y)-rock|Ps],Pn).

fillin((P1X,PY),(P2X,PY),Ps,Pn) :- P1X #> P2X, fillin((P2X,PY),(P1X,PY),Ps,Pn).
fillin((P1X,PY),(P2X,PY),Ps,Pn) :- P1X #< P2X,
    PXn #= P1X+1,
    fillin((PXn,PY),(P2X,PY),[(P1X,PY)-rock|Ps],Pn).


down((X,Y),(X,Yn)) :- Yn #= Y + 1.
down_left((X,Y),(Xn,Yn)) :- Xn #= X - 1, Yn #= Y + 1.
down_right((X,Y),(Xn,Yn)) :- Xn #= X + 1, Yn #= Y + 1.


% is_air(Grid,P,false) :- member(P-rock, Grid).
% is_air(Grid,P,false) :- member(P-sand, Grid).
% is_air(Grid,P, true) :- nonmember(P-sand,Grid,true),nonmember(P-rock,Grid,true).

get_assoc_default(Key,Assoc,Val,Default,Val0) :- 
    ( get_assoc(Key,Assoc,Val), Val0 = Val; Val0=Default).

is_air(Grid,P,false) :- get_assoc(P, Grid, _Val).

%for part 2 add the floor
is_air(_Grid,(_,169),false).
% is_air(_Grid,(_,11),false).
is_air(Grid,P, true) :- not(is_air(Grid,P,false)).

above_abyss(MaxY,(_X,Y),true) :- Y #< MaxY.
above_abyss(MaxY,(_X,Y),false) :- Y #>= MaxY.


sand_falls(_Grid,MaxY,P,abyss) :- above_abyss(MaxY,P,false).

sand_falls(Grid,MaxY,P,P) :- above_abyss(MaxY,P,true),
    down(P,D), down_left(P,DL), down_right(P,DR),
    is_air(Grid,D,false),  is_air(Grid,DL,false), is_air(Grid,DR, false).

sand_falls(Grid,MaxY,P,Dn) :- above_abyss(MaxY,P,true),
    down(P,Down), is_air(Grid,Down,true), 
    sand_falls(Grid,MaxY,Down,Dn).

sand_falls(Grid,MaxY,P,Dn) :- above_abyss(MaxY,P,true),
    down(P,Down), is_air(Grid,Down,false), 
    down_left(P,DL), is_air(Grid,DL, true),
    sand_falls(Grid,MaxY,DL,Dn).

sand_falls(Grid,MaxY,P,Dn) :- above_abyss(MaxY,P,true),
    down(P,Down), is_air(Grid,Down,false), 
    down_left(P,DL), is_air(Grid,DL, false),
    down_right(P,DR), is_air(Grid,DR, true),
    sand_falls(Grid,MaxY,DR,Dn).


loop(Grid,MaxY,Grains,GridN,[P|Grains]) :- 
    sand_falls(Grid,MaxY,(500,0),P),
    put_assoc(P,Grid,sand,GridN).

run_loop(Grid,_MaxY,[abyss|Gs],Grid,[abyss|Gs]).

run_loop(Grid,MaxY,Grains,GridN, GrainsN) :- 
    loop(Grid,MaxY,Grains, Grid1,Grains1),
    run_loop(Grid1,MaxY,Grains1,GridN,GrainsN).

run_loop2(Grid,_MaxY,[(500,0)|Gs],Grid,[(500,0)|Gs]).
run_loop2(Grid,MaxY,Grains,GridN, GrainsN) :- 
    loop(Grid,MaxY,Grains, Grid1,Grains1),!,
    run_loop2(Grid1,MaxY,Grains1,GridN,GrainsN).


run_loop2(0,Grid,_MaxY,Gs,Grid,Gs).
run_loop2(N,Grid,MaxY,Grains,GridN, GrainsN) :-
    Nn#=N-1,
    loop(Grid,MaxY,Grains, Grid1,Grains1),!,
    run_loop2(Nn,Grid1,MaxY,Grains1,GridN,GrainsN).


p1(Ans) :- 
    read_14(Paths), 
    phrase(grid_it_up(Paths),[],Grid0), 
    list_to_set(Grid0,Grid1), 
    list_to_assoc(Grid1,Grid),
    findall(Y,member((_,Y)-rock,Grid0),Ys),
    max_list(Ys,MaxY),
    run_loop(Grid,MaxY,[],GridN,GrainsN),
    length(GrainsN,Len),
    Ans #= Len-1.

p2(Ans) :- 
    read_14(Paths),
    phrase(grid_it_up(Paths),[],Grid0),
    list_to_set(Grid0,Grid1),
    list_to_assoc(Grid1,Grid),
    run_loop2(Grid,500,[],GridN,GrainsN),
    length(GrainsN,Ans).