library(lists).
library(apply).
library(random).
library(pairs).

% Dynamic predicates
:- dynamic population /1. % The current population
:- dynamic listOfGames /1. % The list of games to play
:- dynamic coldness/1. % Current selection pressure




n(1). 
nTeams(20).


setPopulation(Pop):-
    retractall(population),
    asserta(population(Pop):-!),
    !.

populate:-
    n(N),
    populate(0,N,[],Pop),
    setPopulation(Pop),
    !.
populate(N,N,Pop,Pop):-!.
populate(I,N,Temp,Pop):-
    I=<N,
    nTeams(Nteams),
    approx(Nteams,New),
    append([New],Temp,Pop2),
%    [H|T] = Pop2,
%    [A|B] = H,
    I2 is I+1,
    populate(I2,N,Pop2,Pop),
    write(Pop),
    !.

games(N):-
    games(1,1,N,Temp),
    write(Temp),
    !.
games(N,Res):-
    games(1,1,N,Res),
    !.
games(N,B,N,[[N,B,0]]):-
    B=:=(N-1),
    !.
games(A,A,N,Res):-
    sig(A,A,N,A2,B2),
    games(A2,B2,N,Res),
    !.
games(A,B,N,Res):-
    Mylist=[0,19,21,23,25,27,43,45,47,49],
    random_member(X,Mylist),
    sig(A,B,N,A2,B2),
    games(A2,B2,N,Temp),
    append(Temp,[[A,B,X]],Res),
    % list of days 
    % write(X),
    !.
sig(A,N,N,SigA,SigB):-
    SigA is (A+1),
    SigB is 1,
    !.
sig(A,B,_,A,SigB):-
    SigB is (B+1),
    !.

randomAssignment(N,Res):-
    games(N,Temp),
    random_permutation(Temp,Res),
    !.

approx(N,Out):-
    games(N,Remaining),
    K is N/2,
    approx(Remaining,[],K,[],Out),
    !.
approx(Remaining,_,_,Res,Out):-
    length(Remaining,Lr),
    Lr=:=0,
    Out=Res,
    !.
approx(Remaining,Week,K,Res,Out):-
    length(Week,L),
    L=:=K,
    approx(Remaining,[],K,Res,Out),
    !.
approx(Remaining,Week,K,Res,Out):-
    length(Remaining,Lr),
    Lr>0,
    gameToAdd(Remaining,Week,Game),
    % Add to Res
    append(Res,[Game],Res2),
    % Add to Week
    append([Game],Week,Week2),
    % Remove from remaining
    subtract(Remaining,[Game],Remaining2),
    % Recursive call
    approx(Remaining2,Week2,K,Res2,Out),
    !.


gameToAdd(Remaining,Week,Game):-
    available(Remaining,Week,Available),
    length(Available,La),
    La>0,
    random_member(Game,Available),
    !.
gameToAdd(Remaining,_,Game):-
    random_member(Game,Remaining),
    !.
    

gameTeamsInWeek(TeamsInWeek,[A|[B]]):-
    count(TeamsInWeek,A,Ac),
    Ac=:=0,
    count(TeamsInWeek,B,Bc),
    Bc=:=0,
    !.
available(Remaining,Week,Res):-
    flatten(Week,TeamsInWeek),
    include(gameTeamsInWeek(TeamsInWeek),Remaining,Res),
    !.

available(Remaining,Week,Res):-
    flatten(Week,TeamsInWeek),
    include(gameTeamsInWeek(TeamsInWeek),Remaining,Res),
    !.
