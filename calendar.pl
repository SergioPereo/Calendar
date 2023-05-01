:- dynamic distance/3,visited/1,location/2,traveled/2.
:- use_module(library(random)).

link(a,b,30).
link(b,a,30).
link(a,c,80).
link(c,a,80).
link(b,d,50).
link(d,b,50).
link(a,d,40).
link(d,a,40).
link(c,d,60).
link(d,c,60).
link(c,e,30).
link(e,c,30).
link(d,e,50).
link(e,d,50).

distance(X,X,0).

factorial(1,1):-!.
factorial(X,Y) :-
	X > 1,
	X1 is X-1,
	factorial(X1,Y1),
	Y is X * Y1.

/*
combinacion/3: es un predicado que calcula el número de combinaciones
de X elementos tomados de Y en Y. Se usa para calcular P2.
*/
combination(X,Y,C) :-
	factorial(X,Num),
	R is X-Y,
	factorial(R,Facr),
	factorial(Y,Facy),
	Den is Facr * Facy,
	C is Num/Den.

node(N):-link(N,_,_).
node(N):-link(_,N,_).

first_element([First|_],First).

connection(N,B):-link(N,B,_).

connections(N,Connections):-setof(B,(connection(N,B),not(visited(B))),Connections),!.
connections(_,[]).

all_nodes(Nodes):-setof(A,node(A),Nodes),!.
all_nodes([]).

quantity_of_nodes(N):-all_nodes(Nodes),length(Nodes,N).

store(_,_,[],_):-!.
store(Origin,Node,[Connection|Connections],Carry):-
    link(Node,Connection,Distance),
    ActualDistance is Distance+Carry,
    (not(distance(Origin,Connection,_))->
    assertz(distance(Origin,Connection,ActualDistance))
    ;distance(Origin,Connection,StoredDistance),
    (ActualDistance<StoredDistance->retract(distance(Origin,Connection,StoredDistance)),
    assertz(distance(Origin,Connection,ActualDistance));true)),
    store(Origin,Node,Connections,Carry).

dijkstra(Origin,Node,Carry):-
    connections(Node,Connections),
    length(Connections,Len),
    (Len>0->
    store(Origin,Node,Connections,Carry),
    first_element(Connections,Next),
    link(Node,Next,Distance),
    assertz(visited(Node)),
    NewCarry is Distance+Carry,
    dijkstra(Origin,Next,NewCarry),
    retract(visited(Node));
    true).

make_path([]):-!.
make_path([Node|Nodes]):-
    dijkstra(Node,Node,0),
    make_path(Nodes).

make_paths:-
    all_nodes(Nodes),
    make_path(Nodes).

... --> [] | [_], ... .

pair(L,X,Y) :-
    phrase((..., [X], ..., [Y], ...), L).

pairs(List,Pairs):-
    setof([A,B],pair(List,A,B),Pairs).

check_pairs([]):-!.
check_pairs([[A,B|_]|Pairs]):-
    (A==B->write("Error"),nl;true),
    check_pairs(Pairs).

create_match(A,B,Pool,Match):-
    random_member(Location,Pool),
    Match=[[A,A,B],[Location,A,B]].

solution_creator([],_,[]):-!.
solution_creator([[A,B|_]|Pairs],Pool,Solution):-
    create_match(A,B,Pool,Match),
    solution_creator(Pairs,Pool,Matches),
    append(Match,Matches,Solution).

make_random_solution(Solution):-
    all_nodes(Nodes),
    pairs(Nodes,Pairs),
    solution_creator(Pairs,Nodes,Matches),
    random_permutation(Matches,PermutatedM),
    append(PermutatedM,Solution).

check_solution([]):-!.
check_solution([_,A,B|Solution]):-
    (A==B->write("Error"),nl;true),
    check_solution(Solution).


distances([],Traveled,Traveled):-!.
distances([Place,A,B|Solution],Traveled,Distance):-
    (location(Place,A)->
    TravelA = 0;
    (location(ALoc,A)->
    distance(ALoc,Place,TravelA),
    retract(location(ALoc,A)),
    assertz(location(Place,A));
    distance(A,Place,TravelA)),
    assertz(location(Place,A))),
    (location(Place,B)->
    TravelB = 0;
    (location(BLoc,B)->
    distance(BLoc,Place,TravelB),
    retract(location(BLoc,B)),
    assertz(location(Place,B));
    distance(B,Place,TravelB)),
    assertz(location(Place,B))),
    (traveled(A,TA)->
    NewTA is TA+TravelA,
    retract(traveled(A,TA)),
    assertz(traveled(A,NewTA));
    assertz(traveled(A,TravelA))),
    (traveled(B,TB)->
    NewTB is TB+TravelB,
    retract(traveled(B,TB)),
    assertz(traveled(B,NewTB));
    assertz(traveled(B,TravelB))),
    SA is Traveled+TravelA,
    NewTraveled is SA+TravelB,
    distances(Solution,NewTraveled,Distance).



