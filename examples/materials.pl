material(gold).
material(aluminium).
process(bauxite,alumina).
process(alumina,aluminium).
process(copper,bronze).
valuable(X) :- material(X).
valuable(X) :- process(X,Y),valuable(Y).