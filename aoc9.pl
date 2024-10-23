:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(assoc)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(utils).
:- use_module(dcg_utils).


move(x, X-Y, Xn-Y) :- Xn #= X + 1.
move(y, X-Y, X-Yn) :- Yn #= Y + 1.
move(xy1,X-Y,Xn-Yn) :- Xn #= X + 1, Yn #= Y + 1.
move(xy2,X-Y,Xn-Yn) :- Xn #= X + 1, Yn #= Y - 1.

move(down,P,P1)  :- move(y,P1,P).
move(up,P,P1)    :- move(y,P,P1).
move(left,P,P1)  :- move(x,P1,P).
move(right,P,P1) :- move(x,P,P1).

diff(Hx-Hy,Tx-Ty,Dx-Dy) :- Dx #= Hx - Tx, Dy #= Hy - Ty.

catchup(-1- -1, Tail, Tail).
catchup(-1-0, Tail, Tail).
catchup(-1-1, Tail, Tail).
catchup(0- -1, Tail, Tail).
catchup(0-0, Tail, Tail).
catchup(0-1, Tail, Tail).
catchup(1- -1, Tail, Tail).
catchup(1-0, Tail, Tail).
catchup(1-1, Tail, Tail).

catchup(-2- -1,Tail,Tn) :- move(down, Tail, T0), move(left,T0,Tn).
catchup(-2-0,Tail,Tn) :-  move(left, Tail, Tn).
catchup(-2-1,Tail,Tn) :- move(up, Tail, T0), move(left,T0,Tn).

catchup(2- -1,Tail,Tn) :- move(down, Tail, T0),move(right,T0,Tn).
catchup(2-0,Tail,Tn) :- move(right, Tail, Tn).
catchup(2-1,Tail,Tn) :- move(right, Tail, T0), move(up,T0,Tn).

catchup(-1-2,Tail,Tn) :- move(left,Tail,T0), move(up,T0,Tn).
catchup(0-2,Tail,Tn) :- move(up,Tail,Tn).
catchup(1-2,Tail,Tn) :- move(right,Tail,T0), move(up,T0,Tn).

catchup(-1- -2,Tail,Tn) :- move(left,Tail,T0),move(down,T0,Tn).
catchup(0- -2,Tail,Tn) :- move(down,Tail,Tn).
catchup(1- -2,Tail,Tn) :- move(right,Tail,T0), move(down,T0,Tn).

catchup(-2- -2,Tail,Tn) :- move(left,Tail,T0), move(down,T0,Tn).
catchup(-2- 2,Tail,Tn) :- move(left,Tail,T0), move(up,T0,Tn).
catchup(2- -2, Tail,Tn) :- move(right,Tail,T0), move(down,T0,Tn).
catchup(2-2, Tail, Tn) :- move(right,Tail,T0), move(up,T0,Tn).


step(up-X)      --> "U ", integer(X),"\n".
step(down-X)    --> "D ", integer(X),"\n".
step(left-X)    --> "L ", integer(X),"\n".
step(right-X)   --> "R ", integer(X),"\n".


runner([])  --> [].
runner([H|T]) --> runner_(H),runner(T).

runner_(_-0)        --> [].
runner_(Direction-Count)    --> {Count #> 0, Cn #= Count - 1}, stepper(Direction), runner_(Direction-Cn).

stepper(Direction),[Hn,Tn,[Tn|Locs]]  --> [Head,Tail,Locs], {
    move(Direction,Head, Hn), 
    diff(Hn,Tail,Dif),
    catchup(Dif, Tail,Tn)}.

p1(O) :- phrase_from_file(sequence(step, Steps),"d9.txt"), phrase(runner(Steps), [0-0,0-0,[0-0]], [_,_,X]), list_to_set(X,XS), length(XS,O).


runner2([])  --> [].
runner2([H|T]) --> runner2_(H),runner2(T).
runner2_(_-0)        --> [].
runner2_(Direction-Count)    --> {Count #> 0, Cn #= Count - 1}, stepper2(Direction), runner2_(Direction-Cn).

stepper2(Direction),[Hn,K1n,K2n,K3n,K4n,K5n,K6n,K7n,K8n,K9n,[K9n|Locs]]  --> [H,K1,K2,K3,K4,K5,K6,K7,K8,K9,Locs], {
    
    move(Direction,H, Hn),
    prop(Hn,K1,K1n), 
    prop(K1n,K2,K2n),
    prop(K2n,K3,K3n),
    prop(K3n,K4,K4n),
    prop(K4n,K5,K5n),
    prop(K5n,K6,K6n),
    prop(K6n,K7,K7n),
    prop(K7n,K8,K8n),
    prop(K8n,K9,K9n)
    }.

prop(Hn,T,Tn) :-
    diff(Hn,T,Dif),
    catchup(Dif,T,Tn).

p2(O) :- 
    phrase_from_file(sequence(step, Steps),"d9.txt"), 
    phrase(runner2(Steps), [0-0,0-0,0-0,0-0,0-0,0-0,0-0,0-0,0-0,0-0,[0-0]],[_,_,_,_,_,_,_,_,_,_,X]), 
    list_to_set(X,XS), length(XS,O).

