% ----------------------------------------------
% -- | COMP90048 Declarative Programming    | --
% -- | Project 4                            | --
% -- | Name: Geoffrey Ka-Hoi Law            | --
% -- | Student No.: 759218                  | --
% -- | Email: glaw@student.unimelb.edu.au   | --
% ----------------------------------------------

:- module(wumpus, [initialState/5, guess/3, updateState/4]).

% initial state
initialState(NR, NC, XS, YS, state(XS-YS,0,[],Map)) :-
        findall(point(X-Y,none), (between(1,NR,X), between(1,NC,Y)), Map).

% initial guess
guess(state(X0-Y0,0,[],Map0), State, Guess) :-
        State = state(X0-Y0,0,[],Map0),
        Guess = [east].
guess(state(X0-Y0,Energy0,Guess0,Map0), State, Guess) :-
        ( Energy0 < 20 ->
          State = state(X0-Y0,Energy0,Guess0,Map0),
          last(Guess0, LastG),
          chooseDir(X0, Y0, Map0, LastG, Dir),
          append(Guess0, [Dir], Guess)
        ; halt
        ).

updateState(state(X0-Y0,Energy0,_Guess0,Map0), Guess, Feedback, State) :-
        Energy is Energy0 + 1,
        last(Guess, LastG), last(Feedback, LastF),
        updatePos(X0-Y0, LastG, LastF, X-Y),
        updateMap(Map0, X-Y, LastF, Map),
        State = state(X-Y,Energy,Guess,Map),
        writeln(LastF), writeln(State),
        LastF \= wumpus.

chooseDir(X0, Y0, Map, PrevDir, Dir) :-
        ( PrevDir == east,  Y is Y0+1, notVisited(X0-Y, Map) -> Dir = south
        ; PrevDir == south, X is X0-1, notVisited(X-Y0, Map) -> Dir = west
        ; PrevDir == west,  Y is Y0-1, notVisited(X0-Y, Map) -> Dir = north
        ; PrevDir == north, X is X0+1, notVisited(X-Y0, Map) -> Dir = east
        ; Dir = east, writeln("ALL VISITED")
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
updateMap([point(X-Y,none)|Rest], X-Y, LastF, [point(X-Y,LastF)|Rest]) :- 
        updateMap(Rest, X-Y, LastF, Rest).
updateMap([point(I-J,Feedback)|Rest0], X-Y, LastF, [point(I-J,Feedback)|Rest]) :- 
        updateMap(Rest0, X-Y, LastF, Rest).
