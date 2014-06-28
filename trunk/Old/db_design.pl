% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

%cp :- consult(proj). % the student project schema
%ct :- consult(debugdata). % the debugging data
%% The file key1 contains some schema that may be used test the code

e :- edit(normal). % this facilitates the edit, compile, test cycle

%%
%% DATABASE DESIGN TOOLS - Functional dependencies and normalization
%%
%% Copyright 2003 by Anthony Aaby. 
%% This code may be used, reproduced, and modified provided this
%% copyright notice is retained.
%%
%% Programmer: Anthony Aaby
%% Created: 11/12/2003 by A. Aaby
%% Last modified: 12/04 by A. Aaby
%% Language: Prolog
%% Compiler/Interpreter: SWI-Prolog Version 5.1.13
%% Program file: http://cs.wwc.edu/~/aabyan/415/normal
%%
%% Description/Functionality: Database design tools
%%
%% Construct attribute closure wrt a set of functional dependencies
%%    closure(X, FDs, XPlus)   
%% Construct Keys and superkeys:
%%    key(Schema, Key), superKey(Schema, SuperKey)
%% Construct minimal cover for functional dependencies
%%    minimalCover(FDs, MinCover)
%% Test for equivalence between sets of functional dependencies
%%    equivalentFDs(FDs1, FDs2)
%% Determine if a schema is in 1st, 2n, 3rd, and Bryce-Codd normal forms
%%    firstNF(Schema), secondNF(Schema), thirdNF(Schema), isBCNF(Schema)
%% Construct 3NF and BCNF schemas:
%%    thirdNF(Schema, Schemas), bcNF(Schema, Schemas)
%% 

%%%% TODO: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  1. Extend tools to perform normalization for 5NF, 4NF, 2NF, 1NF
%  2. Increase the level of sophistication of the tools.
%  3. Improve the usability of the tools.
%  4. Extend tools to handle additional constraints - types, multivalued dependencies, etc

%%%% DATA REPRESENTATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Attribute - Prolog atom
%  Functional dependency - fd(LHS,RHS) where LHS and RHS are lists of attributes
%  Schema = (ListOfAttributes, ListOfFDs)

rName(Schema,Name) :- Schema=..[Name|_].

fds((_, FDs), FDs).

attributes(Schema, Attributes) :- Schema=(Attributes,_),!. 
attributes(Schema, Attributes) :- Schema=..[_|Attributes],!. 
attributes(Schema, Attributes) :- Attributes = Schema. 

%%%%  FUNCTIONAL DEPENDENCIES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Functional dependencies are constructed from the semantics of the attributes.
%
%  representation: fd(LHS,RHS) where LHS and RHS are lists of attributes
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trivial FD: RHS is a subset of LHS
trivialFD(fd(LHS, RHS)) :- subset(RHS, LHS).
% Non-trivial FD: RHS contains an element not found in LHS
nontrivialFD(fd(LHS, RHS)) :- in(X, RHS), \+ in(X, LHS).
% Completely non-trivial FD: LHS and RHS are disjoint
completeNontrivialFD(fd(LHS, RHS)) :- intersection(LHS,RHS,[]).

%
%%% CombineFDs - combine functional dependencies A->B, A->C; A -> B,C.
%

combineFDs(FDs, ComFDs) :-  extract(fd(A,B), FDs, FDs1),
                            extract(fd(Ap,Bp), FDs1, FDs2),
      		            equalSets(A,Ap),!, union(B,Bp,C),
		            combineFDs([fd(A,C)|FDs2], ComFDs).
combineFDs(FDs, FDs). % no matching LHSs.

%
%%% SplitFDs - split FD A -> BC into FDs A->B, A->C.
%

splitFDs([],[]).
splitFDs([FD|FDs], ListOfFDs) :- splitFD(FD, LFD), splitFDs(FDs, List),
                                 append(LFD, List, ListOfFDs).

splitFD(fd(LHS, RHS), ListOfFDs) :- splitFD3(LHS, RHS, ListOfFDs).

splitFD3(_, [], []). 
splitFD3(LHS, [H|T], [fd(LHS,[H])|FDs]) :- splitFD3(LHS, T, FDs). 

%
%%%% KEYS and SUPERKEYS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% Key: an irreducible superkey

key(Schema,Key) :- attributes(Schema, Attributes), fds(Schema,FDs),
                   irreducible(Attributes, FDs, Attributes, Key).

irreducible(Attributes, FDs, TKey, Key) :- extract(_,TKey,NTKey),
                                       closure(NTKey, FDs, Closure),
                                       equalSets(Attributes, Closure),!,
                                       irreducible(Attributes,FDs,NTKey,Key).
irreducible( _, _, TKey, TKey).

% SuperKey = Set of attributes closed under the FDs. Inefficient!

superKey((Attribs, FDs), SuperKey) :- splitFDs(FDs,LFDs),
                                      subset(SuperKey, Attribs),
                                      closure(SuperKey, LFDs, Closure), 
                                      equalSets(Attribs, Closure).
 
%%%% Minimal Cover %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% minimalCover(FDs, MinCover)  % Minimal set of canonical FDs
% 1. The RHS of every FD has just one attribute
% 2. There are no unnecessary attribites in the LHS
% 3. No FD may be removed

minimalCover(FDs, MinCover) :- splitFDs(FDs, Split),
                               simplifiedLHS(Split, Simplified),
                               minimized(Simplified, MinCover).

% Drop unnecessary attributes in LHS
% An attribute in LHS is unnecessary if its removal provides an
% equivalent set of FDs.

simplifiedLHS(FDs,SFDs) :- simplifiedLHS(FDs, [], SFDs).

simplifiedLHS([],    SFDs, SFDs).
simplifiedLHS([fd([X],Y)|FDs], AFDs, SFDs) :- simplifiedLHS(FDs, [fd([X],Y)|AFDs], SFDs).

simplifiedLHS([FD|FDs],        AFDs, SFDs) :- simplifiedFD(FD, FDs, AFDs, FDp), 
                                              simplifiedLHS([FDp|FDs], AFDs, SFDs).

simplifiedLHS([FD|FDs], AFDs, SFDs) :- simplifiedLHS(FDs, [FD|AFDs], SFDs).

simplifiedFD(fd(X,Y), FDs, AFDs, fd(BX,Y)) :- extract(_,X,BX), 
	        		              append([fd(X,Y)|FDs], AFDs, FDs1),
			                      append([fd(BX,Y)|FDs], AFDs, FDs2),
			                      equivalentFDs(FDs1, FDs2).

% Eliminate redundant FDs
% A FD is not redundant if when dropped the FDs are not equivalent.

minimized(FDs, MinFDs) :- minimized(FDs, [], MinFDs).

minimized([], MinFDs,  MinFDs).
minimized([FD|FDs], PFDs, MinFDs) :- append(FDs,PFDs,SFDs),
                                    \+ equivalentFDs(SFDs, [FD|SFDs]),!,
                                    minimized(FDs, [FD|PFDs], MinFDs).
minimized([_|FDs], PFDs, MinFDs) :- minimized(FDs, PFDs, MinFDs).

%%%% Equivalent FDs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% two sets of FD are equivalent if they have the same covers.

equivalentFDs(E, F) :- covers(E, F),!, covers(F, E).

%%%% F covers E %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FDs F covers FDs E iff every FD in E is inferred by F

covers(_,[]).
covers(F, [FD|E]) :- in(FD,F),!, covers(F, E). % an optimization
covers(F, [fd(X,Y)|E]) :- closure(X,F,XPlus), subset(Y,XPlus),!, covers(F, E).

%%%% Attribute Closure wrt FDs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% closure(X, FDs, XPlus) - XPlus is the closure of X under the FDs.
% where X is a set of attributes and F a set of functional dependencies
% Closure of X is XU{attributes reachable under the FDs}

% no more FDs to use
closure(X, [], XPlus) :- XPlus = X.

% extend X using an FD (the LHS is a subset of X)
closure(X, FDs, XPlus) :- extend(X, FDs, FDsP, XP),!,
                          closure(XP, FDsP, XPlus).
% there is no FD whose LHS is in X
closure(X, _, XPlus) :- XPlus = X.

% extend X using an FD whose LHS is in X
extend(X, FDs, FDsP, XPlus) :- extract(fd(Y,Z), FDs, FDsP),
                               subset(Y, X),!, union(Z, X, XPlus).

%%%% NORMAL FORMS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 5NF, 4NF, BCNF, 3NF, 2NF, 1NF
%
% Representation: Schema = (Attributes, FDs), Schemas = list of schema
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% 5th NF rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Further splitting creates tables that cannot be joined to recreate the original 

% fifthNF( ... )

%%%% 4th NF rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No multivalued dependencies

% fourthNF(...)

%%%% Boyce-Codd NF rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% whenever A -> B, A is a superkey.

%%% bcNF(Schema, BCNF) - using 3NF synthesis followed by decomposition

bcNF(Schema, BCNF) :- thirdNF(Schema,Schemas), 
                      bcNF1(Schemas, BCNF).
% Decompose each FD (bcNF2 decomposes a single FD)
bcNF1([], []).
bcNF1([Schema|Schemas], BCNFs) :- bcNF2(Schema,Ss),
                                  bcNF1(Schemas,S2s),
				  append(Ss, S2s, BCNFs).

%%% Recursive Boyce-Codd Decomposition

% If a FD violates BCNF then recursively decompose it
bcNF2(Schema, BCNF) :- violatesBCNF(Schema, FD),
                       decompose(Schema, FD, S1, S2),
		       bcNF2(S1,S1s), bcNF2(S2,S2s),
		       append(S1s,S2s,BCNF).

% All FDs are in BCNF
bcNF2(Schema, BCNF) :- BCNF = [Schema].

% FD is not BCNF if LHS is not a superkey
violatesBCNF(Schema, fd(LHS, RHS)):- fds(Schema, FDs),
                                     combineFDs(FDs, CFDs),
                                     extract(fd(LHS,RHS), CFDs, _),
                                     \+ superKey(Schema, LHS). 
% Decompose FD into BCNF
decompose(Schema, fd(L,R), S1, S2) :- attributes(Schema, Attributes),
                                      fds(Schema, FDs),
                                      union(L,R,LR), 
                                      applicableFDs(LR, FDs, S1FDs),
		 		      S1 =(LR,S1FDs),
                                      difference(Attributes,R,Diff), 
                                      applicableFDs(Diff, FDs, S2FDs),
				      S2=(Diff, S2FDs).

% applicableFDs(Attributes, FDs, AFDs)

applicableFDs(Attributes, [fd(X,Y)|FDs], [fd(X,Y)|AFDs]) :- 
            subset(X,Attributes),
            subset(Y,Attributes), !,
            applicableFDs(Attributes,FDs, AFDs).

applicableFDs(_, [], []).
applicableFDs(Attributes, [_|FDs], AFDs) :- applicableFDs(Attributes,FDs, AFDs).

% Determine if a schema and FDs satisfy the BCNF 

isBCNF((Attributes, FDs)) :- bcnf(Attributes, FDs, FDs).

bcnf(Attributes, [], FDs) :-
     write(Attributes), write(' and '), write(FDs), write(' satisfy the BCNF.'), nl.

bcnf(Attributes, [fd(LHS,_)|R], FDs) :- superKey((Attributes, FDs), LHS),
                                    bcnf(Attributes, R, FDs).

bcnf(Attributes, [fd(LHS,RHS)|_], _) :-
          write(Attributes), write(' and '), write(fd(LHS,RHS)), 
          write(' violates the BCNF.'), nl.

%%%% 3rd NF rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% whenever A -> B, either A is a superkey or B is a member of a key

% 3rd NF of a schema.

thirdNF(Schema, TNF) :- thirdNFsynthesis(Schema,Schemas),
			remainingAttributes(Schema,Schemas,TNF).

% compute minimal cover, combine FD, and synthesize the 3NF.
thirdNFsynthesis(Schema,Schemas) :- fds(Schema, FDs), 
                                    minimalCover(FDs, MinCover),
			            combineFDs(MinCover,CFDs),
			            synthesis(CFDs, Schemas).

% insures that each attribute is in at least one relation
% (attributes that are not functionally determined by other attributes)
remainingAttributes(Schema, Schemas, TNF) :- attributes(Schema, As),
		                             ra(As, Schemas, [], TNF).

ra([],[],TNF,TNF).
ra(As,[],Temp,[fd(As,[])|Temp]).
ra(As,[(X,Y)|Schemas], Temp, TNF) :- difference(As,X,Diff),
                                     ra(Diff, Schemas, [(X,Y)|Temp],TNF).

% Create a relation from each FD
synthesis([],[]).
synthesis([fd(A,B)|FDs], [(AB,[fd(A,B)])|Schemas]):- union(A,B,AB),
                                                     synthesis(FDs,Schemas).

% Determines if a schema and FDs satisfy the 3NF.

thirdNF(Schema) :- attributes(Schema, Attributes), fds(Schema, FDs),
                   thirdnf(Attributes, FDs, FDs).

thirdnf(Schema, [], FDs) :-
     write(Schema), write(' and '), write(FDs), write(' satisfy the 3NF.'), nl.

% The RHS of the FD is a subset of the LHS
%thirdnf(Schema, [fd(LHS,RHS)|R], FDs) :-
%          subset(RHS, LHS),!, thirdnf(Schema, R, FDs).
% The FD is in 3NF if LHS is a superkey.
thirdnf(Schema, [fd(LHS,_)|R], FDs) :-
          superKey((Schema, FDs), LHS),!, thirdnf(Schema, R, FDs).
% The FD is in 3NF if RHS is a subset of a superkey.
thirdnf(Schema, [fd(_,RHS)|R], FDs) :-
          superKey((Schema, FDs), SKey), subset(RHS, SKey),!,
          thirdnf(Schema, R, FDs).

thirdnf(Schema, [fd(LHS,RHS)|_], _) :- write(Schema), write(' and '),
                                       write(fd(LHS,RHS)),
                                       write(' violate the 3NF.'), nl.

%%%% 2nd NF rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  whenever A -> B, A is not a proper subset of a key.

secondNF(Schema, SNF).

% Determines if a schema, key and FDs satisfy the 2NF
secondNF(Schema) :- attributes(Schema, Attributes),
                    fds(Schema, FDs),
		    key(Schema, Key),
                    secondnf(Attributes, Key, FDs, FDs).

secondnf(Schema, Key, [], FDs) :-
               write(Schema), write(', '), write(Key), 
               write(' and '), write(FDs), write(' satisfy the 2NF.'), nl.

secondnf(Schema, Key, [fd([_],_)|R], FDs) :- !,
                                             secondnf(Schema, Key, R, FDs).
secondnf(Schema, Key, [fd(LHS,_)|R], FDs) :-
          \+ properSubset(LHS, Key),!, secondnf(Schema, Key, R, FDs).

secondnf(Schema, Key, [fd(LHS,RHS)|_], _) :- write(Schema), write(' with '),
                                         write(key(Key)), write(' and '),
                                         write(fd(LHS,RHS)),
                                         write(' violate the 2NF.'), nl.

%%%% 1st NF rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Attributes must be single valued (not a set; singular - child not children)
% 2. Attributes must not be nested (address, name etc)

% A schema, its key, and list of 1NF schemas
firstNF(Schema, Key, Schemas) :- attributes(Schema,Attributes),
                                 rName(Schema,Name),
                                 partition(Attributes, Key, Atomic, Compound),
                                 Primary=..[Name|Atomic],
                                 Schemas = [Primary|Compound].

partition([], _, [], []).
partition([A|As], Key, [A|Atomic], Compound) :- atomic(A), 
                                           partition(As, Key, Atomic, Compound).
partition([C|As], Key, Atomic, [NR|Compound]) :- \+ atomic(C), 
                                           C =..[Name|Attributes],
                                           NR =..[Name,Key|Attributes],
                                           partition(As, Key, Atomic, Compound).

% Determines if a schema is satisfies 1NF

firstNF(Schema) :- write('Only testing for nested attributes.'), nl,
                   attributes(Schema, Attributes), 
                   atomicAttributes(Attributes),
                   write(Schema), write(' satisfies 1NF.').

atomicAttributes([]).
atomicAttributes([H|T]) :- atomic(H), atomicAttributes(T).
atomicAttributes([H|T]) :- write( H ), write(' is not atomic.'), nl,
                           atomicAttributes(T).

%%%% SET OPERATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

