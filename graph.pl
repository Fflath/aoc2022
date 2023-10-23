:- module(graph,[graph/2,clean/0, neighbors/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(utils).


graph(Nodes, Edges) :- 
    phrase(assert_nodes, Nodes),
    phrase(assert_edges, Edges).

clean :- retractall(node(_)), retractall(edge(_)).    

assert_nodes --> [].
assert_nodes --> [N], {assertz(node(N))}, assert_nodes.

assert_edges --> [].
assert_edges --> [N1-N2], {
    node(N1),node(N2),
    assertz(edge(node(N1)-node(N2)))}, assert_edges.

% Take a starting node and list of nodes and initialize the starting distance to 0 for the starting node
% and inf for all others.


init_dist_(Node, node_distance(inf-Node)).

initialize_distance(StartNode,Dist) :- 
    findall(N,node(N),Nodes),
    maplist(init_dist_,Nodes,D0),
    remove(node_distance(inf-StartNode),D0,D1),
    append([node_distance(0-StartNode)],D1,Dist).

% utility

neighbors(Node,Neighbors) :- findall(N, edge(node(Node)-node(N)), Neighbors).

unseen(Dist,Node,true) :- member(node_distance(inf-(Node)),Dist).
unseen(Dist,Node,false).
% unseen(Dist,Node,false) :- member(node_distance(D-(Node)),Dist), dif(D,inf).

update_distance(Distance,Node,Distances,DistancesN) :- 
    remove(node_distance(inf-(Node)),Distances,Distances1),
    append([node_distance(Distance-(Node))],Distances1,DistancesN).


lookup_distance(DistanceMatrix,Node,D) :- member(node_distance(D-(Node)),DistanceMatrix).
% djik

% djik(StartNode,DistN) :-
    
consider_node(Node0,Dist0,DistN) :-
    neighbors(Node0,Neighbors),
    tfilter(unseen(Dist0), Neighbors, Unseen),
    member(node_distance(CurrentDistance-Node0),Dist0),
    Dn #= CurrentDistance+1,
    fold_left(graph:update_distance(Dn),Unseen,Dist0,DistN),!.
    
consider_distance(Distance,Dist0,DistN) :-
    findall(Node,member(node_distance(Distance-Node),Dist0),Nodes),
    fold_left(graph:consider_node,Nodes,Dist0,DistN).

calculate_distance(Stop,_Distance,Dn,Dn) :- member(node_distance(D-(Stop)),Dn),dif(D,inf).

calculate_distance(Stop,Distance,D0,Dn) :-  
    consider_distance(Distance,D0,D1),
    Distance1 #= Distance + 1,
    calculate_distance(Stop,Distance1,D1,Dn),!.



