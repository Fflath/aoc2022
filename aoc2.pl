:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(func)).

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

game([])        --> call(eos), !.
game([R|Rs]) --> round(R), game(Rs).
round((P1,P2))    --> p1(P1), " ", p2(P2), "\n".
eos([], []).

score_list([])  --> [].
score_list([R|Rs])  --> s1(R), score_list(Rs). 
s1((P1, P2)),[Score]            --> [S0],{
    results(P1,P2,R), score(R,RV),
    score(P2, PV),
    Score #= S0 + RV + PV}.

p1 :- phrase_from_file(game(Rs), "d2.txt"), phrase(score_list(Rs),[0], [X]), write(X).


t2(loss)    --> "X".
t2(draw)    --> "Y".
t2(win)     --> "Z".

game2([])        --> call(eos), !.
game2([R|Rs]) --> round2(R), game2(Rs).
round2((P1,P2))    --> p1(P1), " ", t2(P2), "\n".
score_list2([])  --> [].
score_list2([R|Rs])  --> s2(R), score_list2(Rs). 
s2((P1, R)),[Score]            --> [S0],{
    results(P1,P2,R),
    score(R,RV),
    score(P2, PV),
    Score #= S0 + RV + PV}.


p2 :- phrase_from_file(game2(Rs), "d2.txt"), phrase(score_list2(Rs),[0], [X]), write(X).
