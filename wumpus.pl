%%% ---------------------------------------------- %%%
%%% -- | COMP90048 Declarative Programming    | -- %%%
%%% -- | Project 4                            | -- %%%
%%% -- | Name: Geoffrey Ka-Hoi Law            | -- %%%
%%% -- | Student No.: 759218                  | -- %%%
%%% -- | Email: glaw@student.unimelb.edu.au   | -- %%%
%%% ---------------------------------------------- %%%

:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% initial state
initialState(NR, NC, XS, YS, state(XS-YS,XS-YS,[],Map,[])) :-
        findall(point(X-Y,none), (between(1,NR,X), between(1,NC,Y)), Map).

%% initial guess
guess(state(XS-YS,X0-Y0,[],Map0,Shots), State, Guess) :-
        % initially go east and update the coordinates
        Guess = [east],
        X1 is X0 + 1, Y1 is Y0,
        % update State with new coordinates and Guess
        State = state(XS-YS,X1-Y1,Guess,Map0,Shots).
%% guess
guess(state(XS-YS,X0-Y0,Guess0,Map0,Shots), State, Guess) :-
        ( \+ hasWumpus(Map0, _), \+ hasPit(Map0, _) ->
          % guess part one: find the wumpus
          last(Guess0, LastG),
          guessPartOne(X0-Y0, Map0, LastG, Dirns, X1-Y1),
          append(Guess0, Dirns, Guess),
          State = state(XS-YS,X1-Y1,Guess0,Map0,Shots)
        ; hasWumpus(Map0, XW-YW), Shots == [] ->
          % guess part two: generate all possible paths and shoot the wumpus
          guessPartTwo(XS-YS, XW-YW, Map0, Shots0),
          pop(Shots0, First, Shots1),
          makeShoot(First, Guess),
          State = state(XS-YS,X0-Y0,Guess0,Map0,Shots1)
        ; hasWumpus(Map0, _),
          % pop the first path list from Shots list
          pop(Shots, First, Shots1),
          makeShoot(First, Guess),
          State = state(XS-YS,X0-Y0,Guess0,Map0,Shots1)
        ).

%% update state
updateState(state(XS-YS,X0-Y0,_Guess0,Map0,Shots), Guess, Feedback, State) :-
        ( \+ hasWumpus(Map0, _), \+ hasPit(Map0, _) ->
          last(Feedback, LastF),
          updateMap(Map0, X0-Y0, LastF, Map),
          State = state(XS-YS,X0-Y0,Guess,Map,Shots)
          % debug here
          % writeln(LastF), writeln(State)
        ; hasWumpus(Map0, _),
          State = state(XS-YS,X0-Y0,Guess,Map0,Shots)
        ).

guessPartOne(X0-Y0, Map, PrevDir, Dirns, X1-Y1) :-
        ( PrevDir == east,  Y1 is Y0 + 1, X1 is X0, notVisited(X1-Y1, Map) -> Dirns = [south]
        ; PrevDir == south, X1 is X0 - 1, Y1 is Y0, notVisited(X1-Y1, Map) -> Dirns = [west]
        ; PrevDir == west,  Y1 is Y0 - 1, X1 is X0, notVisited(X1-Y1, Map) -> Dirns = [north]
        ; PrevDir == north, X1 is X0 + 1, Y1 is Y0, notVisited(X1-Y1, Map) -> Dirns = [east]
        ; pickNone(Map, X1-Y1), generateOnePath(Map, X0-Y0, X1-Y1, Dirns)
        ).

guessPartTwo(X0-Y0, X1-Y1, Map, Shots) :-
        setof(Path, generateAllPaths(Map, X0-Y0, X1-Y1, Path), Shots).

pop([X|List], X, List).

makeShoot([], []).
makeShoot([_|[]], [shoot|[]]).
makeShoot([Type|Rest0], [Type|Rest]) :- makeShoot(Rest0, Rest).

pickNone([point(X-Y,none)|_], X-Y).
pickNone([_|Rest], X-Y) :-
        pickNone(Rest, X-Y).

notVisited(X-Y, [point(X-Y,none)|_]).
notVisited(X-Y, [point(_,_)|Rest]) :- notVisited(X-Y, Rest).

updateMap([], _, _, []).
updateMap([point(X-Y,_)|Rest], X-Y, LastF, [point(X-Y,LastF)|Rest]) :- 
        updateMap(Rest, X-Y, LastF, Rest).
updateMap([point(I-J,Type)|Rest0], X-Y, LastF, [point(I-J,Type)|Rest]) :- 
        updateMap(Rest0, X-Y, LastF, Rest).

hasPit([point(X-Y,pit)|_], X-Y).
hasPit([point(_,_)|Rest], X-Y) :- hasPit(Rest, X-Y).

hasWumpus([point(X-Y,wumpus)|_], X-Y).
hasWumpus([point(_,_)|Rest], X-Y) :- hasWumpus(Rest, X-Y).

isEdge(_Map, X-Y, X-Y).                 % the end point
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

generateOnePath(Map, Start, End, Path) :-
        generatePath(Map, Start, End, [Start], Path), !.
generateAllPaths(Map, Start, End, Path) :-
        generatePath(Map, Start, End, [Start], Path).
generatePath(_Map, Start, Start, _Previous, []).
generatePath(Map, Start, End, Previous, [Dirn|Path]) :-
        through(Map, Start, Dirn, Med, End),
        \+ member(Med, Previous),
        generatePath(Map, Med, End, [Med|Previous], Path).
