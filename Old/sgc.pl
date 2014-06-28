%%%%%%%%%%%%%%%%%%%
%                 %
% SAME GENERATION %
%                 %
%%%%%%%%%%%%%%%%%%%

person(ann).
person(bertrand).
person(charles).
person(dorothy).
person(evelyn).
person(fred).
person(george).
person(hilary).

par(dorothy,george).
par(evelyn,george).
par(bertrand,dorothy).
par(ann,dorothy).
par(ann,hilary).
par(charles,evelyn).

sgc(X,X) :- person(X).
sgc(X,Y) :- par(X,X1), sgc(X1,Y1), par(Y,Y1).

anc(X,Y) :- par(X,Y).
anc(X,Y) :- anc(X,Z),anc_2(Z,Y).
anc_2(X,Y) :- anc(X,Y).

sgc_anc(X,Y,Z) :- sgc(X,Y), anc(X,Z), anc_2(Y,Z).
sgc_anc(X,Y,Z) :- sgc_anc(X,Y,W), par(W,Z).

% It overloads the memory!!!
