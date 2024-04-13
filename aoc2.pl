:- use_module(library(dcgs)).
:- use_module(dcg_utils).   
:- use_module(library(pio)).
:- use_module(library(lists)).

results(X,X,draw).
results(rock,paper,win).
results(scissors,rock, win).
results(paper, scissors, win).
results(rock,scissors,loss).
results(scissors,paper,loss).
results(paper,rock,loss).

score(loss,0).
score(draw,3).
score(win,6).

score(rock,1).
score(paper,2).
score(scissors,3).


p1(rock)     --> "A".
p1(paper)    --> "B".
p1(scissors) --> "C".

p2(rock)     --> "X".
p2(paper)    --> "Y".
p2(scissors) --> "Z".

round((P1,P2))    --> p1(P1), " ", p2(P2), "\n".
game(G) --> sequence(round,G).

s1((P1,P2),Score) :- 
    results(P1,P2,R), 
    score(R,RV),
    score(P2,PV),
    Score #= RV + PV.

sum_(L,S0,S) :- S #= L + S0.

p1 :-   phrase_from_file(game(Rs), "d2.txt"), 
        maplist(s1, Rs, Ss),
        foldl(sum_,Ss,0,X),
        write(X).


t2(loss)    --> "X".
t2(draw)    --> "Y".
t2(win)     --> "Z".

round2((P1,P2))    --> p1(P1), " ", t2(P2), "\n".
game2(G) --> sequence(round2,G).

s2((P1,R),Score):-
    results(P1,P2,R),
    score(R,RV),
    score(P2, PV),
    Score #= RV + PV.

p2 :-   phrase_from_file(game2(Rs), "d2.txt"), 
        maplist(s2,Rs,Ss),
        foldl(sum_,Ss,0,X),
        write(X).
