%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%			   %
% DEFINITE CLAUSE GRAMMARS %
%                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Integers:


% int/1
% int(i).
% int: integer --> boolean.


int(0).
int(N) :- int(N1), N is N1+1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% First version:

 
% abc/1
% abc(i).
% abc: list --> boolean.


abc(X0-X3) :-   a(N, X0-X1), 
        	b(N, X1-X2),
		c(N, X2-X3).


% a/2
% a(i,i).
% a: integer x list --> boolean


a(0, X0-X0).
a(N, [a|X1]-X2) :- a(N1, X1-X2), N is N1+1.


% idem


b(0, X0-X0).
b(N, [b|X1]-X2) :- b(N1, X1-X2), N is N1+1.


% idem


c(0, X0-X0).
c(N, [c|X1]-X2) :- c(N1, X1-X2), N is N1+1.


% It doesn't work! But, why?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Second version:


% Generates the CSL {a^nb^nc^n | n >= 0}:


% idem for the signatures.


abc1(X0-X3) :-   a1(N, X0-X1), 
	         b1(N, X1-X2),
	         c1(N, X2-X3).

a1(0, X0-X0).
a1((N+1), [a|X1]-X2) :- a1(N, X1-X2).

b1(0, X0-X0).
b1((N+1), [b|X1]-X2) :- b1(N, X1-X2).

c1(0, X0-X0).
c1((N+1), [c|X1]-X2) :- c1(N, X1-X2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Another CSL:


% Generates the CSL {a^mb^nc^md^n | n,m >= 0}?


% idem for the signatures.


abcd2(X0-X4) :-	 a2(M, X0-X1), 
	         b2(N ,X1-X2),
	         c2(M ,X2-X3),
		 d2(N, X3-X4).

a2(0, X0-X0).
a2((M+1), [a|X1]-X2) :- a2(M, X1-X2).

b2(0, X0-X0).
b2((N+1), [b|X1]-X2) :- b2(N, X1-X2).

c2(0, X0-X0).
c2((M+1), [c|X1]-X2) :- c2(M, X1-X2).

d2(0, X0-X0).
d2((N+1), [d|X1]-X2) :- d2(N, X1-X2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Yet another 2 DCG's:


% Generates the language denoted by the RE a*b*c*
% (giving also |w| for any string w of the
% language):


s3(N) --> a3(N1), b3(N2), c3(N3), {N is N1+N2+N3}.

a3(0) --> [].
a3(M) --> [a], a3(M1), {M is M1+1}.

b3(0) --> [].
b3(M) --> [b], b3(M1), {M is M1+1}.

c3(0) --> [].
c3(M) --> [c], c3(M1), {M is M1+1}.


% query goal ex: -> s3(X, [a,b], []).


% Generates the language denoted by the RE a*b*c*
% (no counting):


s4 --> a4, b4, c4.

a4 --> [].
a4 --> [a], a4.

b4 --> [].
b4 --> [b], b4.

c4 --> [].
c4 --> [c], c4.


% query goal ex: -> s3([a,b], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Grammar for a frgament of 
% monadic predicate calculus:


va(x1).
va(x2).
va(x3).


con(c1).
con(c2).
con(c3).


fon(f1).
fon(f2).
fon(f3).


pred(p1).
pred(p2).
pred(p3).


var --> [X], {va(X)}.
cons --> [C], {con(C)}.
fonct --> [F], {fon(F)}.
pred --> [P], {pred(P)}.


ter --> var.
ter --> cons.
ter --> fonct, [<], ter, [>].


for --> for1 ; for2.


for1 --> pred, [<], ter, [>].
for1 --> [all], var, [<], for1 ,[>].
for1 --> [<], for1, [>], [implies], [<], for2, [>].
for1 --> [<], for1, [>], [and], [<], for2, [>].
for1 --> [-], [<], for1, [>].
for1 --> [some], var, [<], for1, [>].
for1 --> [<], for1, [>], [or], [<], for2, [>].


for2 --> pred, [<], ter, [>].
for2 --> [all], var, [<], for2 ,[>].
for2 --> [<], for2, [>], [=>], [<], for1, [>].
for2 --> [<], for2, [>], [&], [<], for1, [>].
for2 --> [-], [<], for2, [>].
for2 --> [some], var, [<], for1, [>].
for2 --> [<], for2, [>], [v], [<], for1, [>].








