:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(reif).
:- use_module(library(dcg/high_order)).
:- use_module(utils).



valve(Code-Flow) --> "Valve ",valve_code(Code)," has flow rate=",integer(Flow),";".

valve_code((C1,C2)) --> letter(C1),letter(C2).
letter(L) --> [L], {member(L,"ABCDEFGHIJKLMNOPQRSTUVWXYZ")}.

connections(Dests) --> " tunnels lead to valves ", sequence(valve_code, ", ",Dests).
connections([Dest]) --> " tunnel leads to valve ", valve_code(Dest).
 
zip([],[],[]).
zip([H1|T1],[H2|T2],[(H1,H2)|T3]) :- zip(T1,T2,T3).
line((Valve-Flow, Valve-Dests)) --> valve(Valve-Flow), connections(Dests),"\n". 

clean :- retractall(valve_flow(_,_)),retractall(src_dest(_,_)), retractall(distance(_,_,_)).

setup(File) :-  
    phrase_from_file(sequence(line,L),File),
    zip(VF,VD,L),
    clean,
    forall(member(V-F,VF),assertz(valve_flow(V,F))),
    forall((member(V-Ds,VD),member(D,Ds)),assertz(src_dest(V,D))),
    findall(Node, valve_flow(Node,_),Nodes),
    setup_distance(Nodes),
    retractall(distance(X,X,0)),
    Start = (65,65),
    assertz(distance(Start,Start,0)).


open_valve(Valve,Time,Pressure) :- 
    Rt #= 30 - Time,
    valve_flow(Valve,Flow),
    Pressure #= Flow * Rt.

action(move-Destination), [(Destination,TimeN,PressureN,seen-[Destination|BeenTo])]   --> [(Location,Time,Pressure,seen-BeenTo)], {
    TimeN #< 30,
    distance(Location,Destination,Dist),
    F #\= 0, valve_flow(Destination,F),
    maplist(dif(Destination), BeenTo),
    TimeN #= Time + Dist + 1,
    open_valve(Destination, TimeN, P0),
    PressureN #= Pressure + P0
}.

initd_(Valve,Valve-inf).
initialize_distance_matrix(DM) :- 
    findall(Valve, valve_flow(Valve,_Flow), Valves),
    maplist(initd_,Valves,D0),
    list_to_assoc(D0,DM).

unseen_neighbors(Node,DM,Neighbors) :-
    findall(Neighbor, 
        (src_dest(Node,Neighbor),gen_assoc(Neighbor,DM,inf)),
        Neighbors).

update_distance(Distance,Node, DM, DMN) :- put_assoc(Node,DM,Distance,DMN).

step(Node,DistanceMatrix,DistanceMatrixN) :- 
    get_assoc(Node, DistanceMatrix, Distance),
    unseen_neighbors(Node, DistanceMatrix, Neighbors),
    Dn #= Distance + 1,
    fold_left(update_distance(Dn),Neighbors, DistanceMatrix, DistanceMatrixN).

next_distance(Distance,DistanceMatrix,DistanceMatrixN) :- 
    findall(Node,gen_assoc(Node,DistanceMatrix,Distance),Nodes),
    fold_left(step,Nodes,DistanceMatrix,DistanceMatrixN).

% calculate the full matrix for a given startnode
calculate_distance(StartNode, DistanceMatrix) :-
    initialize_distance_matrix(DM0),
    put_assoc(StartNode, DM0, 0, DM1),
    cd_(0,DM1,DistanceMatrix).

% stopping condintion (we have traversed to all nodes)
count_unseen(DistanceMatrix,Unseen) :- findall(Key,gen_assoc(Key,DistanceMatrix,inf),Keys),length(Keys,Unseen).
cd_(_Distance, DMN, DMN) :- count_unseen(DMN,0).
cd_(Distance, DM, DMN) :- 
    next_distance(Distance,DM,DM1),
    DistanceN #= Distance + 1,
    cd_(DistanceN,DM1,DMN).

assert_distances(StartNode, DM) :- 
    dif(StartNode,(65,65)),
    forall(
        (
            Flow #> 0,valve_flow(Node,Flow),SFlow #> 0, 
            valve_flow(StartNode,SFlow),gen_assoc(Node, DM, Distance)
        ), 
        assertz(distance(StartNode,Node,Distance))).

assert_distances((65,65), DM) :- 
    forall(
        (
            Flow #> 0,valve_flow(Node,Flow),gen_assoc(Node, DM, Distance)
        ), 
        assertz(distance((65,65),Node,Distance))).

setup_distance([]).
setup_distance([Node|Nodes]) :- 
    calculate_distance(Node,DM),assert_distances(Node,DM),
    setup_distance(Nodes).


p1(A) :- 
    setup("d16.txt"),
    Acc0 = [((65,65),0,0,seen-[])],
    findall(P,(length(As,8),phrase(sequence(action,As),Acc0,[(_,_,P,_)])),Ps),
    max_list(Ps,A).

% symmetry breaking
action2(move-D1), [(p1-(D1,TimeN1), p2-(D2,TimeN2), pressure-PressureN, seen-[D1,D2|BeenTo])]   
    --> [(p1-(L1,T1), p2-(L2,T1),pressure-Pressure, seen-BeenTo)], {
    T1 #< 30,
    F1 #\= 0, F2 #< F1, F2 #\= 0, 
    valve_flow(D1,F1),
    maplist(dif(D1), BeenTo),
    distance(L1,D1,Dist1),
    valve_flow(D2,F2),
    maplist(dif(D2), BeenTo),
    distance(L2,D2,Dist2),
    TimeN1 #= T1 + Dist1 + 1,
    TimeN2 #= T1 + Dist2 + 1,
    open_valve(D1, TimeN1, P1),
    open_valve(D2, TimeN2, P2),
    PressureN #= Pressure + P1 + P2
    % write(L1),nl,write(D1),nl,write(L2),nl,write(D2),nl,nl
}.

action2(move-D1), [(p1-(D1,TimeN), p2-(L2,T2), pressure-PressureN, seen-[D1|BeenTo])]   
    --> [(p1-(L1,T1), p2-(L2,T2),pressure-Pressure, seen-BeenTo)], {
    T1 #=< T2, T1 #< 30,
    F #\= 0, valve_flow(D1,F),
    maplist(dif(D1), BeenTo),
    distance(L1,D1,Dist),
    TimeN #= T1 + Dist + 1,
    open_valve(D1, TimeN, P0),
    PressureN #= Pressure + P0
}.

action2(move-D2), [(p1-(L1,T1), p2-(D2,TimeN), pressure-PressureN, seen-[D2|BeenTo])]   
    --> [(p1-(L1,T1), p2-(L2,T2),pressure-Pressure,seen-BeenTo)], {
    T2 #< T1, T2 #< 30,
    F #\= 0, valve_flow(D2,F),
    maplist(dif(D2), BeenTo),
    distance(L2,D2,Dist),
    TimeN #= T2 + Dist + 1,
    open_valve(D2, TimeN, P0),
    PressureN #= Pressure + P0
}.