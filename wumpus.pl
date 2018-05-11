%%% ---------------------------------------------- %%%
%%% -- | COMP90048 Declarative Programming    | -- %%%
%%% -- | Project 4                            | -- %%%
%%% -- | Name: Geoffrey Ka-Hoi Law            | -- %%%
%%% -- | Student No.: 759218                  | -- %%%
%%% -- | Email: glaw@student.unimelb.edu.au   | -- %%%
%%% ---------------------------------------------- %%%

:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% initial state
initialState(NR, NC, XS, YS, state(XS-YS,[],Map)) :-
        findall(point(X-Y,none), (between(1,NR,X), between(1,NC,Y)), Map).

%% initial guess
guess(state(X0-Y0,[],Map0), State, Guess) :-
        X1 is X0 + 1, Y1 is Y0,
        Guess = [east],
        State = state(X1-Y1,Guess,Map0).
%% guess
guess(state(X0-Y0,Guess0,Map0), State, Guess) :-
        ( \+ hasWumpus(X0-Y0, Map0, _) ->
          % guess part one: find the wumpus
          last(Guess0, LastG),
          guessPartOne(X0-Y0, Map0, LastG, Dirns, X1-Y1),
          append(Guess0, Dirns, Guess),
          State = state(X1-Y1, Guess0, Map0)
        ; hasWumpus(X0-Y0, Map0, X-Y),
          % guess part two: shoot the wumpus
          State = state(X0-Y0, Guess0, Map0),
          writeln(X-Y),
          halt
        ).

%% update state
updateState(state(X0-Y0,_,Map0), Guess, Feedback, State) :-
        ( \+ hasWumpus(X0-Y0, Map0, _) ->
          last(Feedback, LastF),
          updateMap(Map0, X0-Y0, LastF, Map),
          State = state(X0-Y0, Guess, Map),
          % debug here
          writeln(LastF), writeln(State)
        ; hasWumpus(X0-Y0, Map0, X-Y),
          writeln(X-Y)
        ).

pickNone([point(X-Y,none)|_], X-Y).
pickNone([_|Rest], X-Y) :-
        pickNone(Rest, X-Y).

guessPartOne(X0-Y0, Map, PrevDir, Dirns, X1-Y1) :-
        ( PrevDir == east,  Y1 is Y0 + 1, X1 is X0, notVisited(X1-Y1, Map) -> Dirns = [south]
        ; PrevDir == south, X1 is X0 - 1, Y1 is Y0, notVisited(X1-Y1, Map) -> Dirns = [west]
        ; PrevDir == west,  Y1 is Y0 - 1, X1 is X0, notVisited(X1-Y1, Map) -> Dirns = [north]
        ; PrevDir == north, X1 is X0 + 1, Y1 is Y0, notVisited(X1-Y1, Map) -> Dirns = [east]
        ; pickNone(Map, X1-Y1), setof(Path, generatePath(Map, X0-Y0, X1-Y1, Path), [Dirns|_Rest])
        ).

notVisited(X-Y, [point(X-Y,none)|_]).
notVisited(X-Y, [point(_,_)|Rest]) :- notVisited(X-Y, Rest).

updateMap([], _, _, []).
updateMap([point(X-Y,_)|Rest], X-Y, LastF, [point(X-Y,LastF)|Rest]) :- 
        updateMap(Rest, X-Y, LastF, Rest).
updateMap([point(I-J,Type)|Rest0], X-Y, LastF, [point(I-J,Type)|Rest]) :- 
        updateMap(Rest0, X-Y, LastF, Rest).

hasWumpus(X-Y, [point(X-Y,wumpus)|_], X-Y).
hasWumpus(X-Y, [point(_,_)|Rest], WX-WY) :- hasWumpus(X-Y, Rest, WX-WY).

isEdge(_Map, X-Y, X-Y). % the end point
isEdge([point(X-Y,empty)|_], X-Y, _).   % empty point
isEdge([point(X-Y,smell)|_], X-Y, _).   % smell point
isEdge([point(X-Y,stench)|_], X-Y, _).  % stench point
isEdge([point(_,_)|Rest], X-Y, EX-EY) :- isEdge(Rest, X-Y, EX-EY). % otherwise

through(Map, X0-Y0, Dirn, X-Y, EX-EY) :- 
        nextPos(X0-Y0, X-Y, Dirn),
        isEdge(Map, X-Y, EX-EY).

nextPos(X0-Y0, X0-Y, Dirn) :- Y is Y0 + 1, Dirn = south.
nextPos(X0-Y0, X-Y0, Dirn) :- X is X0 + 1, Dirn = east.
nextPos(X0-Y0, X0-Y, Dirn) :- Y is Y0 - 1, Dirn = north.
nextPos(X0-Y0, X-Y0, Dirn) :- X is X0 - 1, Dirn = west.

generatePath(Map, Start, End, Path) :-
        generatePath(Map, Start, End, [Start], Path).
generatePath(_Map, Start, Start, _Previous, []).
generatePath(Map, Start, End, Previous, [Dirn|Path]) :-
        through(Map, Start, Dirn, Med, End),
        \+ member(Med, Previous),
        generatePath(Map, Med, End, [Med|Previous], Path).