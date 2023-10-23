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
:- use_module(graph).

elevation(X) --> [C],{nth1(X,"abcdefghijklmnopqrstuvwxyzSE",C)}.


p1(Distance) :- 
    phrase_from_file(sequence(sequence(elevation),"\n",M0),"d12.txt"), 
    map(M0,M1), start(Start,M1,M2), 
    end(End, M2,M3), 
    aoc_12_nodes(M3,Nodes), 
    aoc_12_edges(M3,Edges), 
    graph(Nodes,Edges),
    graph:initialize_distance(Start,D), 
    graph:calculate_distance(End,0,D,Dn), 
    member(node_distance(Distance-(End)),Dn).

p2(Distance) :- 
    phrase_from_file(sequence(sequence(elevation),"\n",M0),"d12.txt"), 
    map(M0,M1), start(Start,M1,M2), 
    end(End, M2,M3), 
    aoc_12_nodes(M3,Nodes), 
    aoc_12_edges(M3,Edges), maplist(swap_pair,Edges,E2),
    graph(Nodes,E2),
    graph:initialize_distance(End,D), 
    graph:calculate_distance(Start,0,D,Dn),
    assertz(dist_calc(Dn)),
    findall(N, member(N-1,M3),Coords), 
    dist_calc(DM),
    maplist(graph:lookup_distance(DM),Coords,Ds),
    remove(inf,Ds,Ds2),
    min_list(Ds2,Distance).



move(_-[],[]).
move(X-[H|T],[X-H|T2]) :- move(X-T,T2).
adjust(X-(Y-Z),(X-Y)-Z).
map(In,M) :- M = maplist(adjust) $ flatten $ maplist(zip_with_index) $ maplist(move) $ zip_with_index $ In.

location(X-Y-Z)     --> [X-Y-Z].

locations([])   --> [].
locations([L|Ls])   --> sequence(location,L),locations(Ls).


get_location(X-Y-Z),Mn-->
    sequence(location,H),
    location(X-Y-Z),
    sequence(location,T),{
        phrase(locations([H,T]),Mn)
    }.

start(XY,Map,MapN) :- 
    member(XY-27, Map),
    remove(XY-27, Map, Map1),
    append([XY-1],Map1,MapN).

end(XY, Map, MapN) :- 
    member(XY-28, Map),
    remove(XY-28, Map, Map1),
    append([XY-26],Map1,MapN).

sme(Path,Start,Mid,End) :- phrase((location(Start),locations(Mid),location(End)),Path).

transition(Map, X-Y, Xn-Y) :-
    member(X-Y-Z,Map), member(Xn-Y-Zn,Map),
    Dx #= Xn-X, abs(Dx,1),
    Zn #=< Z + 1, Zn #>= 0.

transition(Map, X-Y, X-Yn) :-
    member(X-Y-Z,Map), member(X-Yn-Zn,Map),
    Dy #= Yn-Y, abs(Dy,1),
    Zn #=< Z + 1, Zn #>= 0.


make_edge(Coord1,Coord2,Edge) :- Edge = (Coord1)-(Coord2).
neighbors(Map, Coord, Neighbors) :- findall(N, transition(Map, Coord, N),Neighbors).
aoc_12_edges(Map,Coord,Edges) :- 
    neighbors(Map,Coord,Ns),
    maplist(make_edge(Coord),Ns,Edges).

aoc_12_node(X-Y-_Z,X-Y).
aoc_12_nodes(Map, Nodes) :- maplist(aoc_12_node, Map, Nodes).
aoc_12_edges(Map, Edges) :- 
    aoc_12_nodes(Map,Nodes),
    maplist(aoc_12_edges(Map),Nodes,Es), flatten(Es,Edges).