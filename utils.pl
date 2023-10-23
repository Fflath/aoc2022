:- module(utils,[swap_args/3,(...)/2,
    zip/3,not_nil/2,zip_with_length/2,
    sort_by_length/2,zip_with_index/2,
    iota/2,appendF/3,max/3,remove/3,min/3,
    replace/4,fold_left/4,nonmember/3,swap_pair/2,
    bool_and/3,count/3,absF/2]).

:- use_module(library(func)).
:- use_module(library(clpfd)).

... --> [] | [_], ... .
eos([], []).

swap_args(Pred,A,B) :- call(Pred, B,A).

swap_pair(X-Y,Y-X).

zip([],[],[]).
zip([H1|T1],[H2|T2],[H1-H2|T3]) :- zip(T1,T2,T3).

not_nil(nil,false).
not_nil(X,true) :- dif(X,nil).
   
zip_with_length([],[]).
zip_with_length([H|T],[L-H|T2]) :- length(H,L),zip_with_length(T,T2).

sort_by_length(Ls,SLs) :-
    SLs = zip_with_length $ keysort $ zip_with_length $ Ls.


zip_with_index(L,L1) :- zip_with_index_(L,L1,0).
zip_with_index_([],[],_).
zip_with_index_([H|T],[I-H|T2],I) :- In #= I + 1, zip_with_index_(T,T2,In).


iota(I,Indices) :- Indices = reverse $ iota_ $ I.
iota_(0,[0]).
iota_(N0,[N0|Is]) :- N1 #= N0 - 1, iota_(N1,Is).   



appendF([]) --> [].
appendF([L|Ls]) --> appendF(L), appendF(Ls).


max(inf,_,inf).
max(_,inf,inf).
max(X,Y,X) :- X #>=Y.
max(X,Y,Y) :- X #<Y.

min(inf,Y,Y).
min(X,inf,X).
min(X,Y,Y) :- X #>=Y.
min(X,Y,X) :- X #<Y.


subtract(X,Y,Z) :- Z #= Y-X.
add(X,Y,Z) :- Z #= X + Y.

remove(_,[],[]).
remove(N,[X|Ns],[X|Ns2]) :- dif(N,X), remove(N,Ns,Ns2).
remove(N,[N|Ns],Ns2) :- remove(N,Ns, Ns2).

replace(X,Y,List0,ListN) :- remove(X,List0,List1), append([Y],List1,ListN).

fold_left(Pred, List, Acc, AccN) :- phrase(fold_left_(Pred,List), [(Acc)], [(AccN)]).

fold_left_(_Pred,[]) --> [].
fold_left_(Pred,[I|Is]) --> fold_left__(Pred,I), fold_left_(Pred,Is).
fold_left__(Pred,I), [(AccN)]    --> [(Acc)], {call(Pred,I,Acc,AccN)}.

nonmember(_I,[],true).
nonmember(I,[I|_],false).
nonmember(I,[X|Is],Bool) :- dif(I,X),nonmember(I,Is,Bool).

bool_and(true,true,true).
bool_and(false,_,false).
bool_and(_,false,false).

count(Element, List, Count) :- count(Element,List,0,Count).
count(_Element, [], Count,Count).
count(Element, [Element|Tail], Count,CountN) :- Count1 #= Count + 1, count(Element,Tail,Count1,CountN).
count(Element, [Head|Tail], Count,CountN) :- dif(Element,Head), count(Element,Tail,Count,CountN).

absF(X,Y) :- X#<0,Y#= -1*X.
absF(X,X) :- X#>=0.