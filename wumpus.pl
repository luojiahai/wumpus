%%% ---------------------------------------------- %%%
%%% -- | COMP90048 Declarative Programming    | -- %%%
%%% -- | Project 4                            | -- %%%
%%% -- | Name: Geoffrey Ka-Hoi Law            | -- %%%
%%% -- | Student No.: 759218                  | -- %%%
%%% -- | Email: glaw@student.unimelb.edu.au   | -- %%%
%%% ---------------------------------------------- %%%

:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% initial state
%% initialize map with each coordinate represented as point(X-Y, Type)
initialState(NR, NC, XS, YS, state(XS-YS,XS-YS,[],Map,[])) :-
        findall(point(X-Y,none), (between(1,NC,X), between(1,NR,Y)), Map).

%% guess
guess(state(XS-YS,_,Guess0,Map0,Shots), State, Guess) :-
        ( isWumpus(X1-Y1, Map0), 
          searchTwo(Map0, XS-YS, X1-Y1, Dirns, Shots) ->
          % part two
          makeShoot(Dirns, Guess),
          append(Shots, [Dirns], Shots1),
          State = state(XS-YS,X1-Y1,Guess0,Map0,Shots1)
        ; % part one
          getNones(Map0, Nones), Nones \= [],
          searchOne(Map0, XS-YS, X1-Y1, Nones, Dirns),
          append([], Dirns, Guess),
          State = state(XS-YS,X1-Y1,[],Map0,Shots)
        ).

%% update state
updateState(state(XS-YS,X0-Y0,Guess0,Map0,Shots), Guess, Feedback, State) :-
        ( Guess0 == [] ->
          % part one
          last(Feedback, LastF),
          updateMap(Map0, X0-Y0, LastF, Map),
          State = state(XS-YS,X0-Y0,Guess,Map,Shots),
          % debug here
          writeln(LastF), writeln(State)
        ; % part two
          State = state(XS-YS,X0-Y0,Guess,Map0,Shots),
          % debug here
          last(Feedback, LastF),
          writeln(LastF), writeln(State)
        ).

%% update map
updateMap([], _, _, []).
updateMap([point(X-Y,_)|Rest], X-Y, LastF, [point(X-Y,LastF)|Rest]) :- 
        updateMap(Rest, X-Y, LastF, Rest).
updateMap([point(I-J,Type)|Rest0], X-Y, LastF, [point(I-J,Type)|Rest]) :- 
        updateMap(Rest0, X-Y, LastF, Rest).

%% get all unexplored points in a list of coordinates X-Y that in type 'none'
getNones([], []).
getNones([point(X-Y,none)|Rest0], [X-Y|Rest]) :- getNones(Rest0, Rest).
getNones([_|Rest0], Nones) :- getNones(Rest0, Nones).

%% check whether the map contains a wumpus which in the position X-Y
isWumpus(X-Y, [point(X-Y,wumpus)|_]).
isWumpus(X-Y, [point(_,_)|Rest]) :- isWumpus(X-Y, Rest).

%% check whether a path is a valid path for shooting wumpus
%% check if the last two directions are the same
isValidShot([X,X|[]]).
isValidShot([_|Rest]) :- isValidShot(Rest).

%% replace the last element of a path to 'shoot'
makeShoot([], []).
makeShoot([_|[]], [shoot|[]]).
makeShoot([Type|Rest0], [Type|Rest]) :- makeShoot(Rest0, Rest).

%% check whether if there is a edge between two points
isEdge(_Map, X-Y, X-Y).                 % the end point
isEdge([point(X-Y,empty)|_], X-Y, _).   % empty point
isEdge([point(X-Y,smell)|_], X-Y, _).   % smell point
isEdge([point(X-Y,stench)|_], X-Y, _).  % stench point
isEdge([point(X-Y,damp)|_], X-Y, _).    % damp point
isEdge([point(_,_)|Rest], X-Y, EX-EY) :- 
        isEdge(Rest, X-Y, EX-EY).       % otherwise

%% explore map
explore(Map, X0-Y0, Dirn, X-Y, EX-EY) :- 
        nextPos(X0-Y0, X-Y, Dirn),
        isEdge(Map, X-Y, EX-EY).

%% generate four positions and correspond coordinates
nextPos(X0-Y0, X0-Y, Dirn) :- Y is Y0 + 1, Dirn = south.
nextPos(X0-Y0, X-Y0, Dirn) :- X is X0 + 1, Dirn = east.
nextPos(X0-Y0, X0-Y, Dirn) :- Y is Y0 - 1, Dirn = north.
nextPos(X0-Y0, X-Y0, Dirn) :- X is X0 - 1, Dirn = west.

%% search a path for part one
searchOne(Map, X0-Y0, X1-Y1, [XN-YN|Rest], Path) :-
        ( searchOneHelper(Map, X0-Y0, XN-YN, Path), Path \= [] -> 
          X1 is XN, Y1 is YN
        ; searchOne(Map, X0-Y0, X1-Y1, Rest, Path)
        ).
searchOneHelper(Map, Start, End, Path) :-
        search(Map, Start, End, [Start], Path), !.

%% search a path for part two
searchTwo(Map, Start, End, Path, Sols) :- 
        search(Map, Start, End, [Start], Path), 
        \+ member(Path, Sols), isValidShot(Path), !.

%% search path
search(_Map, Start, Start, _Previous, []).
search(Map, Start, End, Previous, [Dirn|Path]) :-
        explore(Map, Start, Dirn, Med, End),
        \+ member(Med, Previous),
        search(Map, Med, End, [Med|Previous], Path).
