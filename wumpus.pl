%%% ----------------------------------------------
%%% -- | COMP90048 Declarative Programming    | --
%%% -- | Project 4                            | --
%%% -- | Name: Geoffrey Ka-Hoi Law            | --
%%% -- | Student No.: 759218                  | --
%%% -- | Email: glaw@student.unimelb.edu.au   | --
%%% ----------------------------------------------

:- module(wumpus, [initialState/5, guess/3, updateState/4]).

%% initial state
initialState(NR, NC, XS, YS, state(XS-YS,[],Map)) :-
        findall(point(X-Y,none), (between(1,NR,X), between(1,NC,Y)), Map).

%% initial guess
guess(state(X0-Y0,[],Map0), State, Guess) :-
        State = state(X0-Y0,[],Map0),
        Guess = [east].
%% guess
guess(state(X0-Y0,Guess0,Map0), State, Guess) :-
        ( \+ hasWumpus(X0-Y0, Map0, _) ->
          % guess part one: find the wumpus
          State = state(X0-Y0,Guess0,Map0),
          last(Guess0, LastG),
          guessPartOne(X0, Y0, Map0, LastG, Dirns),
          append(Guess0, Dirns, Guess)
        ; hasWumpus(X0-Y0, Map0, X-Y),
          % guess part two: shoot the wumpus
          State = state(X0-Y0,Guess0,Map0),
          halt
        ).

%% update state
updateState(state(X0-Y0,_,Map0), Guess, Feedback, State) :-
        ( \+ hasWumpus(X0-Y0, Map0, _) ->
          last(Guess, LastG), last(Feedback, LastF),
          updatePos(X0-Y0, LastG, LastF, X-Y),
          updateMap(Map0, X-Y, LastF, Map),
          State = state(X-Y,Guess,Map),
          writeln(LastF), writeln(State)
        ; hasWumpus(X0-Y0, Map0, X-Y),
          writeln(X-Y)
        ).

guessPartOne(X0, Y0, Map, PrevDir, [Dirn]) :-
        ( PrevDir == east,  Y is Y0 + 1, notVisited(X0-Y, Map) -> Dirn = south
        ; PrevDir == south, X is X0 - 1, notVisited(X-Y0, Map) -> Dirn = west
        ; PrevDir == west,  Y is Y0 - 1, notVisited(X0-Y, Map) -> Dirn = north
        ; PrevDir == north, X is X0 + 1, notVisited(X-Y0, Map) -> Dirn = east
        ; Dirn = east
        ).

notVisited(X-Y, [point(X-Y,none)|_]).
notVisited(X-Y, [point(_,_)|Rest]) :- notVisited(X-Y, Rest).

updatePos(X0-Y0, LastG, LastF, X-Y) :-
        ( LastG == east,  LastF \= wall -> X is X0 + 1, Y is Y0
        ; LastG == south, LastF \= wall -> X is X0, Y is Y0 + 1
        ; LastG == west,  LastF \= wall -> X is X0 - 1, Y is Y0
        ; LastG == north, LastF \= wall -> X is X0, Y is Y0 - 1
        ; X is X0, Y is Y0
        ).

updateMap([], _, _, []).
updateMap([point(X-Y,_)|Rest], X-Y, LastF, [point(X-Y,LastF)|Rest]) :- 
        updateMap(Rest, X-Y, LastF, Rest).
updateMap([point(I-J,Feedback)|Rest0], X-Y, LastF, [point(I-J,Feedback)|Rest]) :- 
        updateMap(Rest0, X-Y, LastF, Rest).

hasWumpus(X-Y, [point(X-Y,wumpus)|_], X-Y).
hasWumpus(X-Y, [point(_,_)|Rest], WX-WY) :- hasWumpus(X-Y, Rest, WX-WY).

find(Start, End, Path) :-
        find(Start, End, [Start], Path).
find(Start, Start, _Previous, []).
find(Start, End, Previous, [Dirn|Path]) :-
        edge(Start, Dirn, Med),
        \+ member(Med, Previous), % dont visit previous places
        find(Med, End, [Med|Previous], Path).
