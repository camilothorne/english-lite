% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

e :- edit(datalog). % this facilitates the edit, compile, test cycle

%% DATALOG

%% Copyright 2003 by Anthony Aaby. 
%% This code may be used, reproduced, and modified provided this
%% copyright notice is retained.
%%
%% Programmer: Anthony Aaby
%% Last modified: 11/14/2003 by A. Aaby
%% Language: Prolog
%% Compiler/Interpreter: SWI-Prolog Version 5.1.13
%% Program file: /home/aabyan/web/415/normal
%%
%% Description/Functionality: 

%% General format

%% SQL SELECT in Datalog

%%  select SAs from r1(R1As), ..., rn(RnAs) where Conditions(R1As, ..., RnAs).
%% definedRelation(SAs) :- r1(R1As), ..., rn(RnAs), selectionConditions(R1As, ..., RnAs).

%% RELATIONAL ALGEBRA queries in Datalog

%  selectR(As) :- r(As), selectionConditions(As).

%  projR(SubsetOfTheAs) :- r(As).

%  prodR1R2(As Bs) :- r1(As), r2(Bs).

%  unionR1R2(As) :- r1(As); r2(As).
%  or
%    unionR1R2(As) :- r1(As). 
%    unionR1R2(As) :- r2(As).

%  diffR1R2(As) :- r1(As), \+ r2(As).

%  intersectionR1R2(As) :- r1(As), r2(As).

%  divideR1R2(As) :- r1(As Bs), r2(Bs).</blockquote>

%  joinR1R2(UnionOfTheAs) :- r1(R1As), r2(R2As).

%% EXAMPLE

t(2).
t(3).
t(3).
t(5).
t(7).
t(12).
t(52).

%% SQL AGGREGATION OPERATORS in Datalog

ave(Ave) :- bagof(X,X^t(X), L), ave(L,Ave).
aveS(Ave) :- setof(X,X^t(X), L), ave(L,Ave).

%% Max

max([H|T],Max) :- max(T, H, Max).

max([], Max, Max).
max([H|T], TMax, Max) :- H > TMax, max(T, H, Max).
max([H|T], TMax, Max) :- H =< TMax, max(T, TMax, Max).

%% Min

min([H|T],Min) :- min(T, H, Min).

min([], Min, Min).
min([H|T], TMin, Min) :- H < TMin, min(T, H, Min).
min([H|T], TMin, Min) :- H >= TMin, min(T, TMin, Min).

%% Count

count(L,Length) :- count(L, 0, Length).

count([], Length, Length).
count([_|T], PLen, Length) :- PLen1 is PLen + 1, count(T, PLen1, Length).

%% Sum

sum(L,Sum) :- sum(L, 0, Sum).

sum([], Sum, Sum).
sum([H|T], PSum, Sum) :- PSum1 is PSum + H, sum(T, PSum1, Sum).

%% Average

ave(L,Ave) :- average(L, 0,0, Ave).

average([], Count, Sum, Ave) :- Count > 0, Ave is Sum / Count.
average([H|T], PCount, PSum, Ave) :- 
          Count is PCount + 1, 
	  Sum is PSum + H, average(T, Count, Sum, Ave).

%%%% SET OPERATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Computational Set Theory

emptySet([]).
notEmptySet([_|_]).

properSubset(A, B) :- subset(A, B), \+ equalSets(A,B).  

%Subset - a backtracking version
subset([],[]).
subset(SS, Set) :- ground(SS), ground(Set), ggsubset(SS,Set).
subset(SS, Set) :- ground(SS), var(Set), gvsubset(SS,Set).
subset(SS, Set) :- var(SS), ground(Set), vgsubset([], Set, SS).
subset(SS, Set) :- var(SS), var(Set), write('subset(var,var) not implemented'), nl.

ggsubset([], _).
ggsubset([F|R], Set) :- in(F, Set), ggsubset(R, Set).

gvsubset([], _).
gvsubset([F|R], Set) :- append(_, [F|_], Set), gvsubset(R, Set).

vgsubset(SS, _, SS).
vgsubset(TSS, S, SS) :- in(X, S), \+ in(X, TSS), vgsubset([X|TSS], S, SS).

vvsubset([],[_|_]).

selectN(Subset, Set) :- selectN(1, Subset, Set).
selectN(N, Subset, Set) :- subset(Subset, Set), size(Subset, N).
selectN(N, Subset, Set) :- N1 is N+1, selectN(N1, Subset, Set).
size([], 0).
size([_|T], N) :- N1 is N-1, size(T, N1).

equalSets( A, B ) :- subset(A, B), subset(B, A).

difference([], _, []).
difference([X|A], B, Diff) :- in(X, B),!, difference(A, B, Diff).
difference([X|A], B, [X|Diff]) :- \+ in(X, B), difference(A, B, Diff).

union([], [], []).
union([], [F|R], [F|R]).
union([H|T], [], [H|T]).
union([H|T], [F|R], Union) :- in(H, [F|R]), union(T, [F|R], Union).
union([H|T], [F|R], [H|Union]) :- (\+ in(H, [F|R])), union(T, [F|R], Union).

intersection([],[],[]).
intersection([],[_|_],[]).
intersection([_|_],[],[]).
intersection([H|T], [F|R], [H|In]) :- in(H, [F|R]), intersection(T, [F|R], In).
intersection([H|T], [F|R], In) :- \+ in(H,[F|R]), intersection(T, [F|R], In).

in(X, [X|_]).
in(X, [_|L]) :- in(X, L).

% extract X from L leaving LessX - extract(X, L, LessX).

extract(X, L, LessX) :- extract(X, L, [], LessX).
extract(X, [X|L], Prefix, LessX) :- append(Prefix, L, LessX).
extract(X, [Y|L], Prefix, LessX) :- extract(X, L, [Y|Prefix], LessX).

%%%% MISCELANEOUS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeSchema([]).
writeSchema([S|Ss]) :- writeln(S), writeSchema(Ss).

writelist([]).
writelist([H|T]):- writeln(H), writelist(T).

