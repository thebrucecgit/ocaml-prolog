len([], 0).
len([_|X], R) :- len(X, N), R is N + 1.