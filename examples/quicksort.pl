partition(_,[],[],[]).
partition(P, [H|T], [H|L], R) :- H =< P, partition(P, T, L, R).
partition(P, [H|T], L, [H|R]) :- H > P, partition(P, T, L, R).

append([],[],[]).
append([],X,X).
append([H|X], Y, [H|R]) :- append(X, Y, R).

quicksort([],[]).
quicksort([H|X], F) :- partition(H, X, L, R), quicksort(L, LR), quicksort(R, RR), append(LR, [H|RR], F).