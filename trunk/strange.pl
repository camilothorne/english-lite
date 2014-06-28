owes(andy,bill).
owes(bill,carl).
owes(carl,bill).

%%%%%%%%%%%%%%%

avoids(X,Y,L) :- owes(X,Y), \+ member(Y,L).
avoids(X,Y,L) :- owes(X,Z), \+ member(Z,L), avoids(Z,Y,[Z|L]).
