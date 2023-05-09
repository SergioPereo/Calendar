:- dynamic distance/3,visited/1,location/2,traveled/2,roulette/3,solution_data/5,config/3.
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

used_config(L,A,B):-
	config(L,A,B),!.
used_config(L,A,B):-
	config(L,B,A).

create_match(A,B,Match):-
    Match=[[A,A,B],[B,A,B]].

solution_creator([],_,[]):-!.
solution_creator([[A,B|_]|Pairs],Pool,Solution):-
    create_match(A,B,Match),
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

distances_mean(TotalDistance,N,Distances,Mean):-
	findall(Distance,traveled(_,Distance),Distances),
	length(Distances,N),
	Mean is TotalDistance/N.

distances_std([],_,N,Sum,Std):-
	Std is sqrt((Sum/N)).
distances_std([Distance|Distances],Mean,N,Sum,Std):-
	NewSum is Sum+(Distance-Mean)**2,
	distances_std(Distances,Mean,N,NewSum,Std).

solution_heuristic(Run,Solution,Heuristic):-
	distances(Solution,0,Distance),!,
	distances_mean(Distance,N,Distances,Mean),
	distances_std(Distances,Mean,N,0,Std),
	Heuristic is Distance*Std,
	retractall(traveled(_,_)),
	retractall(location(_,_)),
	atomic_list_concat(Solution,Key),
	assertz(solution_data(Key,Run,Distance,Mean,Std)).

make_roulette(_,[],_,HS,HS):-!.
make_roulette(Run,[Solution|Solutions],Index,HeuristicSum,HS):-
	solution_heuristic(Run,Solution,Heuristic),
	NewIndex is Index+1,
	NewHS is HeuristicSum+Heuristic,
	make_roulette(Run,Solutions,NewIndex,NewHS,HS),
	Prob is ((1-(Heuristic/HS))/2),
	atomic_list_concat(Solution,Concat),
	variant_sha1(Concat,Key),
	(roulette(Key,_,_)->
	retract(roulette(Key,_,_)),
	assertz(roulette(Key,Heuristic,Prob));
	assertz(roulette(Key,Heuristic,Prob))
	).


make_population(Max,Max,[]):-!.
make_population(Actual,Quantity,[Solution|Population]):-
	NewActual is Actual+1,
	make_random_solution(Solution),
	make_population(NewActual,Quantity,Population).

select_one(Solutions,Selected):-
	random_member(S,Solutions),
	atomic_list_concat(S,Concat),
	variant_sha1(Concat,Key),
	roulette(Key,_,Prob),
	(maybe(Prob)->
	Selected=S;
	select_one(Solutions,Selected)
	).

get_aleles([],_,_,_,[]):-!.
get_aleles([Location,A,B|S],L,U,Index,Alele):-
	NI is Index+1,
	(between(L,U,Index)->
	Selected=[Location,A,B],
	assertz(config(Location,A,B));
	Selected=[]
	),
	get_aleles(S,L,U,NI,Tail),
	append(Selected,Tail,Alele).

present_alele(Location,A,B):-
       used_config(Location,A,B).

get_complement_aleles([],[]):-!.
get_complement_aleles([Loc,A,B|S],ComplementAleles):-
	(present_alele(Loc,A,B)->
	Selected=[];
	Selected=[Loc,A,B]
	),
	get_complement_aleles(S,Tail),
	append(Selected,Tail,ComplementAleles),
	!.

cross_genes(S1,S2,Child):-
	length(S2,N),
	Matches is N/3,
	Mid is Matches/2,
	Upper is Matches-3,
	random_between(3,Mid,L),
	SLow is Mid+1,
	random_between(SLow,Upper,U),
	get_aleles(S2,L,U,1,Aleles),
	get_complement_aleles(S1,CAleles),
	retractall(config(_,_,_)),
	append(Aleles,CAleles,Child).

mutate(InitialL,InitialA,InitialB,[Location,A,B|Rest],I,I,Location,A,B,[InitialL,InitialA,InitialB|Rest]):-!.
mutate(InitialL,InitialA,InitialB,[Location,A,B|S],Index,SI,SwapL,SwapA,SwapB,[Location,A,B|Rest]):-
	NI is Index+1,
	mutate(InitialL,InitialA,InitialB,S,NI,SI,SwapL,SwapA,SwapB,Rest),
	!.

mutate_thunk([Location,A,B|S1],Index,SI,[SwapL,SwapA,SwapB|Tail]):-
	mutate(Location,A,B,S1,Index,SI,SwapL,SwapA,SwapB,Tail).

mutation(S,MutationProb,Mutation):-
	(maybe(MutationProb)->
	length(S,N),
	NF is N/3,
	random_between(2,NF,R),
	mutate_thunk(S,1,R,M),
	mutation(M,MutationProb,Mutation);
	Mutation=S).

cross(S1,S2,CrossProb,MutationProb,Child1,Child2):-
	(maybe(CrossProb)->
	cross_genes(S1,S2,Ch1),
	cross_genes(S2,S1,Ch2),
	mutation(Ch1,MutationProb,M1),
	mutation(Ch2,MutationProb,M2),
	Child1=M1,
	Child2=M2;
	Child1=S1,
	Child2=S2),
	!.

crossing(_,Crosses,Crosses,[]):-!.
crossing(Solutions,Cross,Crosses,[Ch1,Ch2|NewPopulation]):-
	NewCross is Cross+1,
	select_one(Solutions,S1),
	select_one(Solutions,S2),
	cross(S1,S2,0.7,0.01,Ch1,Ch2),
	crossing(Solutions,NewCross,Crosses,NewPopulation),
	!.

crossover(Solutions,NewPopulation):-
	length(Solutions,N),
	Crosses is N/2,
	crossing(Solutions,0,Crosses,NewPopulation).

best_solution([],Best,Best,_):-!.
best_solution([[Key,Distance,Mean,Std|_]|Solutions],ActualBest,FinalBest,BestSolution):-
	NextHeuristic is Distance*Std,
	(NextHeuristic<ActualBest->
	best_solution(Solutions,NextHeuristic,FinalBest,BS);
	best_solution(Solutions,ActualBest,FinalBest,BS)
	),
	(var(BS)->(FinalBest==NextHeuristic->BestSolution = [Key,Distance,Mean,Std];true);
	BestSolution=BS).

best_solution_thunk([[Key,Distance,Mean,Std|_]|Solutions],BestSolution):-
	InitialBest is Distance*Std,
	best_solution(Solutions,InitialBest,_,BS),
	(var(BS)->BestSolution=[Key,Distance,Mean,Std];BestSolution=BS).

history(Generation,Generation,Population,BestSolution):-
	make_roulette(Generation,Population,0,0,_),
	setof([Key,Distance,Mean,Std],solution_data(Key,Generation,Distance,Mean,Std),Bag),
	best_solution_thunk(Bag,BestSolution),
	!.
history(Generation,LastGeneration,Population,BestSolution):-
	NewGeneration is Generation+1,
	make_roulette(Generation,Population,0,0,_),
	crossover(Population,NewPopulation),
	history(NewGeneration,LastGeneration,NewPopulation,BestSolution).

niflheim(PopulationSize,Generations,BestSolution):-
	make_population(0,PopulationSize,Population),
	history(0,Generations,Population,BestSolution),
	retractall(config(_,_,_)),retractall(roulette(_,_,_)).




