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

directory(Dirs, Files).


no_zero((_-0),false).
no_zero((_-X),true) :- dif(X,0).

command(up)             --> "$ cd ..\n".
command(down(Dir))      --> "$ cd ", string(D),"\n",{string_to_atom(D,Dir)}.
command(nil)            --> "$ ls\n".
file(file(Size))        --> integer(Size), " ", string(_),"\n".
dir(nil)                --> string(_),"\n".
line(L) --> command(L) | file(L) | dir(L).

run_log([]),[(DirList-Size)|Hist] --> [(DirList-Size,Hist)].
run_log([R|Rs])                              --> run(R),run_log(Rs).
run(down(Dir)), [([Dir|DirList]-0,[DirList-Size|Hist])]   --> [(DirList-Size,Hist)].
run(up),        [(DirList-0, [[Cd|DirList]-Size|Hist])]   --> [([Cd|DirList]-Size,Hist)].
run(file(S)),   [(DirList-Sn,Hist)]     --> [(DirList-Size,Hist)], {Sn #= Size+S}.



p1(Out) :- 
    phrase_from_file((sequence(line, Ls)),"d7.txt"), 
    tfilter(not_nil,Ls,L1), 
    phrase(run_log(L1),[(""-0,[])],Y),
    tfilter(no_zero,Y,Ds),
    list_to_assoc(Ds,O),
    assoc_to_keys(O,Ks), 
    reverse(Ks,Ks2),
    phrase(adder1(Ks2),[O],[O2]),
    assoc_to_values(O2,Vs), 
    tfilter(less_than_10k, Vs, Fs), 
    sum_list(Fs,Out).

p2(Out) :- 
    phrase_from_file((sequence(line, Ls)),"d7.txt"),    
    tfilter(not_nil,Ls,L1),
    phrase(run_log(L1),[(""-0,[])],Y),
    tfilter(no_zero,Y,Ds),
    % list_to_assoc(Ds,O),
    zip(Ks1,_,Y),
    K = reverse $ sort_with(length) $ list_to_set $ Ks1,
    empty_assoc(A),
    phrase(init(K), [A],[B]),
    phrase(set(Ds), [B],[C]),
    phrase(adder2(K),[C],[D]),
    assoc_to_values(D,Vs),
    Out = nth1(1) $ sort $ tfilter(big_enough) $ Vs.

add_up1([D|Ds], Alist, Clist) :- add_up1_([D|Ds],Ds,Alist,Clist).

add_up1_(_, [], Clist, Clist).
add_up1_(Key, Ds, Alist, Clist) :- 
    get_assoc(Key, Alist,S1), (get_assoc(Ds,Alist,S2);S2=0),
    Size #= S1+S2,
    put_assoc(Ds,Alist,Size,Blist),
    length(Drop,1),
    append(Drop,Dsn,Ds),
    add_up1_(Key,Dsn,Blist,Clist).

add_up2([[/]],A,A).
add_up2([D|Ds],Alist,Blist) :-
    % write([D|Ds]),nl,
    get_assoc([D|Ds], Alist,S1), 
    get_assoc(Ds,Alist,S2),
    Size #= S1+S2,
    put_assoc(Ds,Alist,Size,Blist).

adder1([[/]])               --> [].
adder1([K|Ks])           --> add1(K),adder1(Ks).
add1(K),[Blist]          --> [Alist], {add_up1(K,Alist,Blist)}.

adder2([[]])            --> [].
adder2([K|Ks])           --> add2(K),adder2(Ks).
add2(K),[Blist]          --> [Alist], {add_up2(K,Alist,Blist)}.

less_than_10k(V,true) :- V #=< 100000.
less_than_10k(V,false) :- V #>= 100000.

init([]) --> [].
init([K|Ks]) --> init_(K),init(Ks).
init_(K),[B] --> [A],{put_assoc(K,A,0,B)}.

set([]) --> [].
set([H|T]) --> set_(H),set(T).
set_((K-V)),[B] --> [A],{put_assoc(K,A,V,B)}. 


% max_list(Vs,Max), FreeSpace #= 70000000 - Max, Need #= 30000000 - FreeSpace.
big_enough(V,true) :- V #> 2558312.
big_enough(V,false) :- V #< 2558312.
