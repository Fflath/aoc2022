:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(pio).
:- use_module(library(clpfd)).
:- use_module(reif).
:- use_module(library(dcg/high_order)).
:- use_module(utils).


action(left)    --> "<".
action(right)    --> ">".
newline-->"\n".

read_input(File,Input) :- phrase_from_file((sequence(action,Input),newline),File).

shape(plus  ,[3-0, 2-1, 3-1, 4-1, 3-2]).
shape(square,[2-0, 3-0, 2-1, 3-1]).
shape(barh  ,[2-0, 3-0, 4-0, 5-0]).
shape(barv  ,[2-0, 2-1, 2-2, 2-3]).
shape(revL  ,[4-0, 4-1, 4-2, 3-2, 2-2]).

order([barh,plus,revL,barv,square]).


addX(Dx, X-Y,Xn-Y) :- Xn #= X + Dx.
addY(Dy, X-Y,X-Yn) :- Yn #= Y + Dy.

move_left(Map, Shape0,Shape1,true)    :- 
    maplist(addX(-1),Shape0,Shape1), 
    is_rock(Map, Shape1, false).

move_left(Map, Shape0,Shape0,false)    :- 
    maplist(addX(-1),Shape0,Shape1), 
    is_rock(Map, Shape1, true).

move_right(Map, Shape0,Shape1,true)    :- 
    maplist(addX(1),Shape0,Shape1), 
    is_rock(Map, Shape1, false).

move_right(Map, Shape0,Shape0,false)    :- 
    maplist(addX(1),Shape0,Shape1), 
    is_rock(Map, Shape1, true).

move_down(Map,Shape0,Shape1,true)    :- 
    maplist(addY(-1),Shape0,Shape1), 
    is_rock(Map, Shape1, false).

move_down(Map,Shape0,Shape0,false)    :- 
    maplist(addY(-1),Shape0,Shape1), 
    is_rock(Map, Shape1, true).

is_rock_(_Map,X-_Y,true) :- X #< 0.
is_rock_(_Map,X-_Y,true) :- X #> 6.
is_rock_(_Map,_X-0,true).
is_rock_(Map,XY,true)    :- get_assoc(XY,Map,rock).
is_rock_(Map,X-Y,false)  :- X in 0..6, Y #> 0, \+ get_assoc(X-Y,Map,_).

is_rock(_Map,[],false).
is_rock(Map,[XY|XYs],Bool) :- is_rock_(Map,XY,false), is_rock(Map,XYs,Bool).
is_rock(Map,[XY|_XYs],true) :- is_rock_(Map,XY,true).



s_action(left),  [(map-Map,spawn-Nn,height-H,shape-Shape1)] --> [(map-Map,spawn-Nn,height-H,shape-Shape0)], {move_left(Map,Shape0,Shape1,_)}.
s_action(right), [(map-Map,spawn-Nn,height-H,shape-Shape1)] --> [(map-Map,spawn-Nn,height-H,shape-Shape0)], {move_right(Map,Shape0,Shape1,_)}.
s_action(down),  [(map-Map,spawn-Nn,height-H,shape-Shape1)] --> [(map-Map,spawn-Nn,height-H,shape-Shape0)], {move_down(Map,Shape0,Shape1,true)}.

s_action(down),  [(map-MapN,spawn-Nn,height-NewH)] --> [(map-Map,spawn-Nn,height-H,shape-Shape0)], {move_down(Map,Shape0,Shape1,false),
    zip(Xs,Ys,Shape1),
    max_list(Ys, MaxY),
    MaybeH #= MaxY + 3,
    max(MaybeH,H,NewH),
    add_shape(Map,Shape,MapN)
}.

s_action(spawn),  [(map-Map,spawn-Nn,height-H,shape-Shape0)]   --> [(map-Map, spawn-N, height-H)], {
    S #= N mod 5, order(O), nth0(S,O,Shape), shape(Shape,XYs),maplist(addY(H),XYs,Shape0),
    Nn #= N + 1
}.


add_shape(Map,Shape,MapN) :- fold_left(add_shape_,Shape,Map,MapN).
add_shape_(Map,XY,MapN) :- put_assoc(XY,Map,rock,MapN).