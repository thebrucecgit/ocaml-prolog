fact(0, 1).
fact(N, M) :- N > 0, K is N - 1, fact(K, R), M is N * R.