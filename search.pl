
%% find a simple Path from Start to End
find(Start, End, Path) :-
	find(Start, End, [Start], Path).
%% find(Start, End, Previous, Path).
%% find a simple Path from Start to End
%% having visited Previous already
find(Start, Start, _Previous, []).
find(Start, End, Previous, [Dirn|Path]) :-
    edge(Start, Dirn, Med),
    \+ member(Med, Previous), % dont visit previous places
    find(Med, End, [Med|Previous], Path).

%% map represented as facts
%% a - b - c
%%     |   |
%%     d - e - f
%%     |   |   |
%%     g   h - i 
edge(1-1,  east, 1-2).
edge(1-2,  west, 1-1).
edge(1-2, south, 2-2).
edge(1-2,  east, 1-3).
edge(1-3,  west, 1-2).
edge(1-3, south, 2-3).
edge(2-2, north, 1-2).
edge(2-2,  east, 2-3).
edge(2-2, south, 3-2).
edge(2-3,  west, 2-2).
edge(2-3, north, 1-3).
edge(2-3,  east, 2-4).
edge(2-3, south, 3-3).
edge(2-4,  west, 2-3).
edge(2-4, south, 3-4).
edge(3-2, north, 2-2).
edge(3-3, north, 2-3).
edge(3-3,  east, 3-4).
edge(3-4,  west, 3-3).
edge(3-4, north, 2-4).
