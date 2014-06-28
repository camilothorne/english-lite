%%%%%%%%%%%%%%%
%
%   DCG FOR
%  LITE ENGLISH
%
%%%%%%%%%%%%%%%

% Resources (lexicon) and modules.

:- consult(dl_fun_lexicon2).
:- consult(dl_con_lexicon2).
:- use_module(sem_dl,[logform/3]).
:- use_module(lambda_abs,[abs_f/2,
			  f_app/6,
			  f_abs/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% N.B. We recall the signature of the semantic functions/relations:

% 1. logform(-exp1,-exp2,+exp3)
% (computes the beta reduct exp3 of exp1 applied to exp2)

% 2. abs_f(-exp,+lam(var,exp))
% (computes the immediate lambda abstraction)

% 3. f_abs(-con1,-type1,+con2,+imp(type2,type1)):
% (a.k.a. --> introduction)
% (type2 is in con2)

% 4. f_app(-con1,-imp(type1,type2),-con2,-type1,+con3,+type2):
% (a.k.a. --> elimination)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A. Phrase structure rules:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sentences (main and subordinated):

% Rem: the gap feature marks (if 'yes')
% subordination

s(sem:S,type:T,con:G,gap:Y,ass:X) -->
	np(sem:NP,type:T1,con:G1,pos:subj,gap:Y,ass:X,coord:_),
	vp(sem:VP,type:T2,con:G2,pos:pred,ass:X,pol:_,voice:_),
	{logform(NP,VP,S)}, % lambda application + beta-conversion
	{f_app(G1,T1,G2,T2,G,T)}. % type application

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Noun phrases:

np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	det(sem:Det,type:T1,con:G1,pos:X,ass:Y),
        noun(sem:N,type:T2,con:G2),
	{logform(Det,N,NP)},
	{f_app(G1,T1,G2,T2,G,T)}.

np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	pnoun(sem:PN,type:T1,con:G1,pos:X,ass:Y),
	{NP = PN,
	 T = T1,
	 G = G1}.

np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	pro(sem:Pro,type:T1,con:G1,pos:X,ass:Y),
	{NP = Pro,
	 T = T1,
	 G = G1}.

% Attention: coordination!

np(sem:NP,type:T,con:G,pos:subj,gap:no,ass:Y,coord:yes) -->
	pro(sem:Pro,type:T1,con:G1,pos:subj,ass:Y),
	coord(sem:C,type:T2,con:G2,pos:subj),
	{logform(C,Pro,NP)},
	{f_app(G2,T2,G1,T1,G,T)}.

np(sem:NP,type:T,con:G,pos:subj,gap:no,ass:Y,coord:yes) -->
	det(sem:Det,type:T1,con:G1,pos:subj,ass:Y),
	noun(sem:N,type:T2,con:G2),
	coord(sem:C,type:T3,con:G3,pos:subj),
	{logform(C,N,L),
	 logform(Det,L,NP),
	 f_app(G3,T3,G2,T2,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.


%%%%%%


% does not work!

/*
np(sem:NP,type:T,con:G,pos:pred,gap:no,ass:Y,coord:no) -->
	pro(sem:Pro,type:T1,con:G1,pos:subj,ass:Y),
	cp(sem:CP,type:T2,con:G2,gap:yes,type:c), %gap!
	{logform(Pro,CP,NP),
	 f_app(G1,T1,G2,T2,G,T)}.*/

%%%%%


%does not work?


np(sem:NP,type:T,con:G,pos:subj,gap:no,ass:Y,coord:no) -->
	pro(sem:Pro,type:T1,con:G1,pos:subj,ass:Y),
	compl(sem:CP,type:T2,con:G2,pos:subj), %gap!
	{logform(Pro,CP,NP),
	 f_app(G1,T1,G2,T2,G,T)}.

/*
np(sem:NP,type:T,con:G,pos:X,gap:yes,ass:tbox,coord:no) -->
	dummy(sem:D,type:T1,con:G1,pos:X,ass:tbox),
	{NP = D,
	 T = T1,
	 G = G1}.*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Negation (only on the right/predicate position):


% Rem: Negation is restricted to subcomponents
% of VP's in predicate position due to DL-Lite
% syntax.


negp(sem:N,type:T,con:G,ass:tbox,pos:pred) -->
	[is],
	neg(sem:Neg1,type:T1,con:G1),
	adj(sem:A,type:T2,con:G2,t:att),
	{logform(Neg1,A,N),
	 f_app(G1,T1,G2,T2,G,T)}.


negp(sem:N,type:T,con:G,ass:tbox,pos:pred) -->
	[is],
	neg(sem:Neg1,type:T1,con:G1),
	[a],
	noun(sem:A,type:T2,con:G2),
	{logform(Neg1,A,N),
	 f_app(G1,T1,G2,T2,G,T)}.


negp(sem:Neg,type:T,con:G,ass:tbox,pos:pred) -->
	[does],
	neg(sem:Neg1,type:T1,con:G1),
	v(sem:V,type:T2,con:G2,cat:trans,pos:pred,ass:tbox,pol:neg,voice:act),
	pro(sem:Pro,type:T3,con:G3,pos:pred,ass:tbox),
	{logform(V,Pro,L),
	 logform(Neg1,L,Neg),
	 f_app(G2,T2,G3,T3,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.


negp(sem:Neg,type:T,con:G,ass:tbox,pos:pred) -->
	[is],
	neg(sem:Neg1,type:T1,con:G1),
	v(sem:V,type:T2,con:G2,cat:trans,pos:pred,ass:tbox,pol:neg,voice:pass),
	[by],
	pro(sem:Pro,type:T3,con:G3,pos:pred,ass:tbox),
	{logform(V,Pro,L),
	 logform(Neg1,L,Neg),
	 f_app(G2,T2,G3,T3,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.


negp(sem:Neg,type:T,con:G,ass:tbox,pos:pred) -->
	[does],
	neg(sem:Neg1,type:T1,con:G1),
	v(sem:V,type:T2,con:G2,cat:intrans,pos:pred,ass:tbox,pol:neg,voice:act),
	{logform(Neg1,V,Neg),
	 f_app(G1,T1,G2,T2,G,T)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Verb phrases:


vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:Z,voice:act) -->
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:Z,voice:act),
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:Z,voice:pass) -->
	[is],
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:Z,voice:pass),
	[by],
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:NP,type:T,con:G,pos:X,ass:Y,pol:Z,voice:W) -->
	v(sem:V,type:T1,con:G1,cat:intrans,pos:X,ass:Y,pol:Z,voice:W),
	{NP = V,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:pred,ass:_,pol:plus,voice:_) -->
	[is,a],
	noun(sem:N,type:T1,con:G1),
	{VP = N,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:pred,ass:_,pol:plus,voice:_) -->
	[is],
	adj(sem:A,type:T1,con:G1,t:att),
	{VP = A,
	 T = T1,
	 G = G1}.

vp(sem:VP,type:T,con:G,pos:Y,ass:X,pol:neg,voice:_) -->
	negp(sem:Neg,type:T1,con:G1,ass:X,pos:Y),
	{VP = Neg,
	 T = T1,
	 G = G1}.

/*
vp(sem:VP,type:T,con:G,pos:X,ass:tbox,pol:_,voice:_) -->
	coord(sem:C,type:T1,con:G1,pos:X),
	{VP = C,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:X,ass:tbox,pol:_,voice:_) -->
	[is],
	coord(sem:C,type:T1,con:G1,pos:X),
	{VP = C,
	 T = T1,
	 G = G1}.*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Complements of nouns in subject position:


compl(sem:Co,type:T,con:G,pos:subj) -->
	[who],
	v(sem:V,type:T1,con:G1,cat:intrans,pos:subj,ass:tbox,pol:plus,voice:act),
	{Co = V,
	 T = T1,
	 G = G1}.


compl(sem:Co,type:T,con:G,pos:subj) -->
	[who,is,a],
	noun(sem:N,type:T1,con:G1),
	{Co = N,
	 T = T1,
	 G = G1}.


compl(sem:Co,type:T,con:G,pos:subj) -->
	[who,is],
	adj(sem:A,type:T1,con:G1,t:att),
	{Co = A,
	 T = T1,
	 G = G1}.


compl(sem:Co,type:T,con:G,pos:subj) -->
	[who,is],
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:tbox,pol:plus,voice:pass),
	[by],
	pro(sem:Pro,type:T2,con:G2,pos:subj,ass:tbox),
	{logform(V,Pro,Co),
	 f_app(G1,T1,G2,T2,G,T)}.


compl(sem:Co,type:T,con:G,pos:subj) -->
	[who],
	v(sem:V,type:T1,con:G1,cat:trans,pos:subj,ass:tbox,pol:plus,voice:act),
	pro(sem:Pro,type:T2,con:G2,pos:subj,ass:tbox),
	{logform(V,Pro,Co),
	 f_app(G1,T1,G2,T2,G,T)}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Left conjunction (on subject position)



% Rem: Coordination on the head of sentences
% doesn't involve NP or VP components
% due to subordination that has to avoided.
% Left hand side DL-Lite concepts are
% not a recursive set!



coord(sem:C,type:T,con:G,pos:subj) -->
	conj(sem:CF,type:T1,con:G1,t:em),
	compl(sem:Co,type:T2,con:G2,pos:subj),
	{logform(CF,Co,C),
	 f_app(G1,T1,G2,T2,G,T)}.


coord(sem:C,type:T,con:G,pos:subj) -->
	conj(sem:CF,type:T1,con:G1,t:em),
	compl(sem:Co,type:T2,con:G2,pos:subj),
	[and],
	coord(sem:C1,type:T3,con:G3,pos:subj),
	{logform(C1,Co,L),
	 logform(CF,L,C),
	 f_app(G3,T3,G2,T2,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Right conjunction (on predicate position)

/*

coord(sem:C,type:T,con:G,pos:pred) -->
	[a],
	noun(sem:N,type:T1,con:G1),
	conj(sem:CF,type:T2,con:G2,t:full),
	vp(sem:VP,type:T3,con:G3,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(CF,VP,L),
	 logform(L,N,C),
	 f_app(G3,T2,G2,T3,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.




coord(sem:C,type:T,con:G,pos:pred) -->
	adj(sem:A,type:T1,con:G1,t:att),
	conj(sem:CF,type:T2,con:G2,t:full),
	vp(sem:VP,type:T3,con:G3,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(CF,VP,L),
	 logform(L,A,C),
	 f_app(G3,T2,G2,T3,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.



coord(sem:C,type:T,con:G,pos:pred) -->
	neg(sem:Neg1,type:T1,con:G1),
	[a],
	noun(sem:N,type:T2,con:G2),
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP,type:T4,con:G4,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(Neg1,N,L1),
	 logform(CF,VP,L2),
	 logform(L2,L1,C),
	 f_app(G1,T1,G2,T2,GG,TT),
	 f_app(G3,T3,G4,T4,GGG,TTT),
	 f_app(GGG,TTT,GG,TT,G,T)}.



coord(sem:C,type:T,con:G,pos:pred) -->
	neg(sem:Neg1,type:T1,con:G1),
	adj(sem:A,type:T2,con:G2,t:att),
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP,type:T4,con:G4,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(CF,VP,L1),
	 logform(Neg1,A,L2),
	 logform(L1,L2,C),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


% they don't work!


coord(sem:C,type:T,con:G,pos:pred) -->
	v(sem:V,type:T1,con:G1,cat:intrans,pos:pred,ass:tbox,pol:pos,voice:act),
	conj(sem:CF,type:T2,con:G2,t:full),
	vp(sem:VP,type:T3,con:G3,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(CF,VP,L),
	 logform(L,V,C),
	 f_app(G2,T2,G3,T3,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.


coord(sem:C,type:T,con:G,pos:pred) -->
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:tbox,pol:pos,voice:act),
	np(sem:NP,type:T2,con:G2,pos:pred,gap:no,ass:tbox,coord:no),
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP,type:T4,con:G4,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(CF,VP,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,C),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


coord(sem:C,type:T,con:G,pos:pred) -->
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:tbox,pol:pos,voice:act),
	np(sem:NP,type:T2,con:G2,pos:pred,gap:no,ass:tbox,coord:no),
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP,type:T4,con:G4,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(CF,VP,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,C),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


coord(sem:C,type:T,con:G,pos:pred) -->
	[does],
	neg(sem:Neg,type:T1,con:G1),
	v(sem:V,type:T2,con:G2,cat:itrans,pos:pred,ass:tbox,pol:neg,voice:act),
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP,type:T4,con:G4,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(Neg,V,L1),
	 logform(CF,VP,L2),
	 logform(L2,L1,C),
	 f_app(G1,T1,G2,T2,GG,TT),
	 f_app(G3,T3,G4,T4,GGG,TTT),
	 f_app(GGG,TTT,GG,TT,G,T)}.



coord(sem:C,type:T,con:G,pos:pred) -->
	[does],
	neg(sem:Neg,type:T1,con:G1),
	v(sem:V,type:T2,con:G2,cat:trans,pos:pred,ass:tbox,pol:neg,voice:act),
	np(sem:NP,type:T3,con:G3,pos:pred,gap:no,ass:tbox,coord:no),
	conj(sem:CF,type:T4,con:G4,t:full),
	vp(sem:VP,type:T5,con:G5,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(V,NP,L0),
	 logform(Neg,L0,L1),
	 logform(CF,VP,L2),
	 logform(L2,L1,C),
	 f_app(G2,T2,G3,T3,GG,TT),
	 f_app(G1,T1,GG,TT,GGG,TTT),
	 f_app(G4,T4,G5,T5,GGGG,TTTT),
	 f_app(GGGG,TTTT,GGG,TTT,G,T)}.



coord(sem:C,type:T,con:G,pos:pred) -->
	[does],
	neg(sem:Neg,type:T1,con:G1),
	v(sem:V,type:T2,con:G2,cat:trans,pos:pred,ass:tbox,pol:neg,voice:act),
	np(sem:NP,type:T3,con:G3,pos:pred,gap:no,ass:tbox,coord:no),
	conj(sem:CF,type:T4,con:G4,t:full),
	vp(sem:VP,type:T5,con:G5,pos:pred,ass:tbox,pol:_,voice:_),
	{logform(V,NP,L0),
	 logform(Neg,L0,L1),
	 logform(CF,VP,L2),
	 logform(L2,L1,C),
	 f_app(G2,T2,G3,T3,GG,TT),
	 f_app(G1,T1,GG,TT,GGG,TTT),
	 f_app(G4,T4,G5,T5,GGGG,TTTT),
	 f_app(GGGG,TTTT,GGG,TTT,G,T)}.*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subordinated clauses:


cp(sem:CP,type:T,con:G,gap:X,t:Z) -->
	relp(sem:C,type:T1,con:G1,t:Z),
	s(sem:S,type:T2,con:G2,gap:X,ass:tbox), %subordinated sentence
	{abs_f(S,S1),
	 f_abs(G2,T2,GG,TT),
	 logform(C,S1,CP),
 	 f_app(G1,T1,GG,TT,G,T)}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nouns with qualificative adjectives (modifiers):

noun(sem:N, type:T,con:G) -->
	adj(sem:A,type:T1,con:G1,t:qual),
	noun(sem:N1,type:T2,con:G2),
	{logform(A,N1,N),% lambda application
	 f_app(G1,T1,G2,T2,G,T)}.% type construction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% B. Categories:

noun(sem:N,type:T,con:G) -->
	[X], {noun(N,T,G,X)}.

det(sem:Det,type:T,con:G,pos:Pos,ass:Ass) -->
	[X], {det(Det,T,G,Pos,Ass,X)}.

pro(sem:Pro,type:T,con:G,pos:Pos,ass:Ass) -->
	[X], {pro(Pro,T,G,Pos,Ass,X)}.

pnoun(sem:PN,type:T,con:G,pos:Pos,ass:Ass) -->
	[X], {pnoun(PN,T,G,Pos,Ass,X)}.

v(sem:S,type:T,con:G,cat:Cat,pos:Pos,ass:Ass,pol:Pol,voice:V) -->
	[X], {v(S,T,G,Cat,Pos,Ass,Pol,V,X)}.

relp(sem:C,type:T,con:G,t:Y) -->
	[X],{relp(C,T,G,Y,X)}.

dummy(sem:D,type:T,con:G,pos:Pos,ass:Ass) -->
	[],{dummy(D,T,G,Pos,Ass)}.

conj(sem:C,type:T,con:G,t:em) -->
	[], {conj2(C,T,G)}.

conj(sem:C,type:T,con:G,t:full) -->
	[X], {conj1(C,T,G,X)}.

adj(sem:A,type:T,con:G,t:Y) -->
	[X],{adj(A,T,G,Y,X)}.

neg(sem:N,type:T,con:G) -->
	[X], {neg(N,T,G,X)}.









