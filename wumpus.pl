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
        findall(point(X-Y,none), (between(1,NC,X), between(1,NR,Y)), Map).

%% initial guess
guess(state(XS-YS,X0-Y0,[],Map0,Shots), State, Guess) :-
        % initially go east and update the coordinates
        Guess = [east],
        X1 is X0 + 1, Y1 is Y0,
        % update State with new coordinates and Guess
        State = state(XS-YS,X1-Y1,Guess,Map0,Shots).
%% guess
guess(state(XS-YS,X0-Y0,Guess0,Map0,Shots), State, Guess) :-
        ( \+ isWumpus(X0-Y0, Map0), \+ isPit(X0-Y0, Map0) ->
          last(Guess0, LastG),
          guessPartOne(X0-Y0, Map0, LastG, Dirns, X1-Y1),
          append(Guess0, Dirns, Guess),
          State = state(XS-YS,X1-Y1,Guess0,Map0,Shots)
        ; isWumpus(X0-Y0, Map0) ->
          guessPartTwo(XS-YS, X0-Y0, Map0, Dirns, Shots),
          makeShoot(Dirns, Guess),
          append(Shots, [Dirns], Shots1),
          State = state(XS-YS,X0-Y0,Guess0,Map0,Shots1)
        ; isPit(X0-Y0, Map0),
          last(Guess0, LastG),
          guessPartOne(XS-YS, Map0, LastG, Dirns, X1-Y1),
          append([], Dirns, Guess),
          State = state(XS-YS,X1-Y1,Guess0,Map0,Shots)
        ).

%% update state
updateState(state(XS-YS,X0-Y0,_Guess0,Map0,Shots), Guess, Feedback, State) :-
        ( \+ isWumpus(X0-Y0, Map0), \+ isPit(X0-Y0, Map0) ->
          last(Guess, LastG),
          last(Feedback, LastF),
          updatePos(X0-Y0, LastG, LastF, X1-Y1),
          updateMap(Map0, X0-Y0, LastF, Map),
          State = state(XS-YS,X1-Y1,Guess,Map,Shots),
          % debug here
          writeln(LastF), writeln(State)
        ; State = state(XS-YS,X0-Y0,Guess,Map0,Shots),
          % debug here
          last(Feedback, LastF),
          writeln(LastF), writeln(State)
        ).

guessPartOne(X0-Y0, Map, PrevDir, Dirns, X1-Y1) :-
        ( PrevDir == east,  Y1 is Y0 + 1, X1 is X0, notVisited(X1-Y1, Map) -> Dirns = [south]
        ; PrevDir == south, X1 is X0 - 1, Y1 is Y0, notVisited(X1-Y1, Map) -> Dirns = [west]
        ; PrevDir == west,  Y1 is Y0 - 1, X1 is X0, notVisited(X1-Y1, Map) -> Dirns = [north]
        ; PrevDir == north, X1 is X0 + 1, Y1 is Y0, notVisited(X1-Y1, Map) -> Dirns = [east]
        ; generateOnePath(Map, Map, X0-Y0, X1-Y1, [], Dirns), write("GEN: "), writeln(Dirns)
        ).

guessPartTwo(X0-Y0, X1-Y1, Map, Dirns, Shots) :-
        generateAllPaths(Map, X0-Y0, X1-Y1, Dirns, Shots).

pop([X|List], X, List).

makeShoot([], []).
makeShoot([_|[]], [shoot|[]]).
makeShoot([Type|Rest0], [Type|Rest]) :- makeShoot(Rest0, Rest).

notVisited(X-Y, [point(X-Y,none)|_]).
notVisited(X-Y, [point(_,_)|Rest]) :- notVisited(X-Y, Rest).

updatePos(X0-Y0, east, wall, X1-Y0) :- X1 is X0 - 1.
updatePos(X0-Y0, south, wall, X0-Y1) :- Y1 is Y0 - 1.
updatePos(X0-Y0, west, wall, X1-Y0) :- X1 is X0 + 1.
updatePos(X0-Y0, north, wall, X0-Y1) :- Y1 is Y0 + 1.
updatePos(X0-Y0, _, _, X0-Y0).

updateMap([], _, _, []).
updateMap([point(X-Y,_)|Rest], X-Y, LastF, [point(X-Y,LastF)|Rest]) :- 
        updateMap(Rest, X-Y, LastF, Rest).
updateMap([point(I-J,Type)|Rest0], X-Y, LastF, [point(I-J,Type)|Rest]) :- 
        updateMap(Rest0, X-Y, LastF, Rest).

isPit(X-Y, [point(X-Y,pit)|_]).
isPit(X-Y, [point(_,_)|Rest]) :- isPit(X-Y, Rest).

isWumpus(X-Y, [point(X-Y,wumpus)|_]).
isWumpus(X-Y, [point(_,_)|Rest]) :- isWumpus(X-Y, Rest).

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

generateOnePath([point(X-Y,none)|Rest], Map, X0-Y0, X-Y, Picks, Path) :-
        ( \+ member(X-Y, Picks), generatePath(Map, X0-Y0, X-Y, [X0-Y0], Path), Path \= [] -> !
        ; generateOnePath(Rest, Map, X0-Y0, X-Y, [X-Y|Picks], Path)
        ).
generateOnePath([_|Rest], Map, X0-Y0, X-Y, Picks, Path) :-
        generateOnePath(Rest, Map, X0-Y0, X-Y, Picks, Path).
generateAllPaths(Map, Start, End, Path, Sols) :- 
        ( generatePath(Map, Start, End, [Start], Path), \+ member(Path, Sols) -> !
        ; generateAllPaths(Map, Start, End, Path, Sols)
        ).
generatePath(_Map, Start, Start, _Previous, []).
generatePath(Map, Start, End, Previous, [Dirn|Path]) :-
        through(Map, Start, Dirn, Med, End),
        \+ member(Med, Previous),
        generatePath(Map, Med, End, [Med|Previous], Path).
