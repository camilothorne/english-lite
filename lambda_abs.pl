%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          %
%			   %
%    Lambda Abstraction	   %
%      and other HOL       %
%       Operations         %
%                          %
%			   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% We need to know when a variable is
% free or bound and then abstract on
% unbound variables that may or may not
% occur in the body of the lambda expression!



% Module



:-module(lam_abs,[abs/2,
		  vars/2,
		  abs_f/2,
		  u_gen/2,
		  u_gen_f/2,
		  e_gen/2,
		  e_gen_f/2,
		  free/2,
		  freevars/2,
		  con/1,
		  f_abs/4,
		  f_app/6,
		  suppr/3,
		  elemr/2,
		  carr/2,
		  compr/2,
		  miroirr/2]).



%%%%%%%%%%%%%%%%%



% Lambda abstraction over (all) free variables



abs(T1,T2):- T1==T2,
	     !,
	     fail.
abs(T1, T2):- freevars(T1, L), 
	      compr(L, FV),
	      miroirr(FV,FV1),
	      abs_iter(T1, T1, FV1, T2).
abs_iter(T1, Acc1, [X|Acc2], T2):- abs_iter(T1, lam(X,Acc1), Acc2, T2).
abs_iter(_, Acc1, [], Acc1):- !.




%%%%%%%%%%%%%%%%%



% Lambda abstraction over the first (bottommost in 
% the parse tree) variable



abs_f(T,lam(X,T)):- freevars(T,L),
		    miroirr(L,[X|_]).


%%%%%%%%%%%%%%%%



% Universal generalization over the first (bottommost in 
% the parse tree) variable



u_gen_f(T,all(X,T)):- freevars(T,L),
		      miroirr(L,[X|_]).



%%%%%%%%%%%%%%%%



% Existential generalization over the first (bottommost in 
% the parse tree) variable



e_gen_f(T,some(X,T)):- freevars(T,L),
		       miroirr(L,[X|_]).




%%%%%%%%%%%%%%%%




% Universal generalization over (all) free variables
%
% To what extent is this possible in HOL?
%
% This corresponds to the universal closure rule!



u_gen(T1,T2):- T1==T2,
	     !,
	     fail.
u_gen(T1, T2):- freevars(T1, L), 
	      compr(L, FV), 
	      u_gen_iter(T1, T1, FV, T2).
u_gen_iter(T1, Acc1, [X|Acc2], T2):- u_gen_iter(T1, all(X,Acc1), Acc2, T2).
u_gen_iter(_, Acc1, [], Acc1):- !.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Existential generalization over (all) free variables
%
% To what extent is this possible in HOL?



e_gen(T1,T2):- T1==T2,
	     !,
	     fail.
e_gen(T1, T2):- freevars(T1, L), 
	      compr(L, FV), 
	      e_gen_iter(T1, T1, FV, T2).
e_gen_iter(T1, Acc1, [X|Acc2], T2):- e_gen_iter(T1, some(X,Acc1), Acc2, T2).
e_gen_iter(_, Acc1, [], Acc1):- !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Checking if a term is a constant



con(T):- atom(T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Checking if a variable is free


% X is free in T iff X e FV(T).


free(X,T):- freevars(T,L), elemr(L,X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Computing a term's free variables



% FV(c) = {}.
% FV(X) = {X}.
% FV(phi(X1,...,XN) = {X1,...,XN}. (we need here 
%                                   to decompose 
%                                   the term!)
% FV(app(T,T') = FV(T) U FV(T').
% FV(lam(X,T) = FV(T) - {X}.
% FV(some(X,T) = FV(T) - {X}.
% FV(all(X,T) = FV(T) - {X}.
% FV(and(T,T') = FV(T) U FV(T').
% FV(not(T)) = FV(T).



freevars(X,[]):- con(X),!.
freevars(X,[X]):- var(X),!.
freevars(app(T1,T2),R):- freevars(T1,L1),
	                 freevars(T2,L2),
		         append(L1,L2,R).
freevars(lam(X,T),R):- freevars(T,L),
	               suppr(X,L,R).
freevars(some(X,T),R):- freevars(T,L),
	                suppr(X,L,R).
freevars(and(T1,T2),R):-freevars(T1,L1),
	                freevars(T2,L2),
		        append(L1,L2,R).
freevars(imp(T1,T2),R):-freevars(T1,L1),
	                freevars(T2,L2),
		        append(L1,L2,R).
freevars(all(X,T),R):- freevars(T,L),
	               suppr(X,L,R).
freevars(not(T),R):- freevars(T,R).



% Free variables in non-ground atoms.


freevars(T,R):- T=..TL,
	        TL = [Pred|RR],
		\+ Pred = lam,
		\+ Pred = app,
		\+ Pred = some,
		\+ Pred = all,
		\+ Pred = not,
		\+ Pred = and,
		atom(Pred),
		vars(RR,R).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Typing rules


% f_abs(-con1,-type1,+con2,+imp(type1,type2))

% f_app(-con1,-imp(type1,type2),-con2,-type1,+con3,+type2)


f_app(G1,imp(T1,T2),G2,T1,G,T2):- append(G1,G2,G).

f_abs([X:T1|G],T2,G,imp(T1,T2)):- var(X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          %
%			   %
%    Auxiliary programs	   %
%                          %
%			   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Used above



suppr(_, [], []):-!. % deleting variables
suppr(X, [T], []):- X==T,!.
suppr(X, [T|Q], L):- X==T,!, 
	             suppr(X, Q, L).
suppr(X, [T|Q], [T|L]) :- suppr(X, Q, L).

%%%%%

elemr(L, X):- carr(L, X). % checking if a variable belongs
elemr([_|Q], X):- elemr(Q, X). % to a list of variables

%%%%%

carr([X|_], Y):- X==Y,!. % checking if a variable is the head of
                         % a list of variables

%%%%%

compr([ ], [ ]). % eliminating repetitions
compr([X|Q], L):- elemr(Q, X),!, 
	          compr(Q, L).
compr([X|Q], [X|L]):- compr(Q, L).


%%%%%

miroirr(L, LL):- miroirr(L, LL, [ ]). % inverting a list
miroirr([ ], LL, Acc):- Acc = LL.
miroirr([T|Q], LL, Acc):- var(T), P = [T|Acc] ,
	                  miroirr(Q, LL, P).



%%%%

vars([X],[X]):- var(X).
vars([X],[]):- atom(X).
vars([X|L],[X|L1]):- var(X), vars(L,L1).
vars([X|L],L1):- atom(X), vars(L,L1).