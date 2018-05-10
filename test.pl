append1([], C, C).
append1([A|B], C, [A|BC]) :-
    append1(B, C, BC).

rev1([], []).
rev1([A|BC], CBA) :-
    rev1(BC, CB),
    append1(CB, [A], CBA).

samelength([], []).
samelength([_|Xs], [_|Ys]) :-
    samelength(Xs, Ys).

rev3(ABC, CBA) :-
    samelength(ABC, CBA),
    rev1(ABC, CBA).

fact(N, F) :-
    ( N =:= 0 ->
        F = 1
        ; N > 0,
        N1 is N - 1,
        fact(N1, F1),
        F is F1 * N
    ).