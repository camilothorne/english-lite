%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%
%    DCG FOR
%  LITE-ENGLISH
%
%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%

:- consult(dl_fun_lexicon_lite).
:- consult(dl_con_lexicon_lite).
:- use_module(sem_dl,[logform/3]).
:- use_module(lambda_abs,[abs_f/2,f_app/6,f_abs/4]).

% 1. logform(-exp1,-exp2,+exp3)
% (computes the beta reduct exp3 of exp1 applied to exp2)

% 2. abs_f(-exp,+lam(var,exp))
% (computes the immediate lambda abstraction)

% 3. f_abs(-con1,-type1,+con2,+imp(type2,type1)):
% (i.e., --> introduction)
% (type2 is in con2)

% 4. f_app(-con1,-imp(type1,type2),-con2,-type1,+con3,+type2):
% (i.e., --> elimination)

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A. Phrase structure rules:
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sentences:
%%%%%%%%%%%%

% Rem: the gap feature marks (if 'yes') subordination

s(sem:S,type:T,con:G,gap:Y,ass:X) -->
	np(sem:NP,type:T1,con:G1,pos:subj,gap:Y,ass:X,coord:_),
	vp(sem:VP,type:T2,con:G2,pos:pred,ass:X,pol:_,voice:_),
	{logform(NP,VP,S)}, % lambda application + beta-conversion
	{f_app(G1,T1,G2,T2,G,T)}. % type application

% Noun phrases:
%%%%%%%%%%%%%%

% ABox

np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	pnoun(sem:PN,type:T1,con:G1,pos:X,ass:Y),
	{NP = PN,
	 T = T1,
	 G = G1}.

% TBox

np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	det(sem:Det,type:T1,con:G1,pos:X,ass:Y),
        nom(sem:N,type:T2,con:G2),
	{logform(Det,N,NP)},
	{f_app(G1,T1,G2,T2,G,T)}.

np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	pro(sem:Pro,type:T1,con:G1,pos:X,ass:Y),
	{NP = Pro,
	 T = T1,
	 G = G1}.

%Att: coordination!

np(sem:NP,type:T,con:G,pos:subj,gap:no,ass:Y,coord:yes) -->
	det(sem:Det,type:T1,con:G1,pos:subj,ass:Y),
	nom(sem:N,type:T2,con:G2),
	[who],
	coord(sem:C,type:T3,con:G3,pos:subj),
	{logform(C,N,L),
	 logform(Det,L,NP),
	 f_app(G3,T3,G2,T2,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.

np(sem:NP,type:T,con:G,pos:subj,gap:no,ass:Y,coord:yes) -->
	pro(sem:Pro,type:T1,con:G1,pos:subj,ass:Y),
	[who],
	coord(sem:CP,type:T2,con:G2,pos:subj),
	{logform(Pro,CP,NP),
	f_app(G1,T1,G2,T2,G,T)}.

% Complements of nouns/pronouns in subject position:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compl(sem:Co,type:T,con:G,pos:subj) -->
	v(sem:V,type:T1,con:G1,cat:intrans,pos:subj,ass:tbox,pol:plus,voice:act),
	{Co = V,
	 T = T1,
	 G = G1}.

compl(sem:Co,type:T,con:G,pos:subj) -->
	[is,a],
	nom(sem:N,type:T1,con:G1),
	{Co = N,
	 T = T1,
	 G = G1}.

compl(sem:Co,type:T,con:G,pos:subj) -->
	[is],
	adj(sem:A,type:T1,con:G1,t:att),
	{Co = A,
	 T = T1,
	 G = G1}.

compl(sem:Co,type:T,con:G,pos:subj) -->
	[is],
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:tbox,pol:plus,voice:pass),
	[by],
	pro(sem:Pro,type:T2,con:G2,pos:subj,ass:tbox),
	{logform(V,Pro,Co),
	 f_app(G1,T1,G2,T2,G,T)}.

compl(sem:Co,type:T,con:G,pos:subj) -->
	v(sem:V,type:T1,con:G1,cat:trans,pos:subj,ass:tbox,pol:plus,voice:act),
	pro(sem:Pro,type:T2,con:G2,pos:subj,ass:tbox),
	{logform(V,Pro,Co),
	 f_app(G1,T1,G2,T2,G,T)}.

% Left conjunction (on subject position)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rem: Coordination on the head of sentences
% doesn't involve NP or VP components

coord(sem:C,type:T,con:G,pos:subj) -->
	conj(sem:CF,type:T1,con:G1,t:em),
	compl(sem:Co,type:T2,con:G2,pos:subj),
	{logform(CF,Co,C),
	 f_app(G1,T1,G2,T2,G,T)}.

coord(sem:C,type:T,con:G,pos:subj) -->
	compl(sem:Co,type:T1,con:G1,pos:subj),
	[and],
	coord(sem:C1,type:T2,con:G2,pos:subj),
	{logform(C1,Co,C),
	 f_app(G2,T2,G1,T1,G,T)}.

coord(sem:C,type:T,con:G,pos:subj) -->
	conj(sem:CF,type:T1,con:G1,t:em),	
	compl(sem:Co,type:T2,con:G2,pos:subj),
	[and],
	coord(sem:C1,type:T3,con:G3,pos:subj),
	{logform(C1,Co,CC),
	 logform(CF,CC,C),
	 f_app(G3,T3,G2,T2,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.

% Verb phrases (only in predicate postion, no coordination):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Negation (only on the predicate position):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Nouns with qualificative adjectives (modifiers):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nom(sem:N, type:T, con:G) -->
	noun(sem:N,type:T,con:G).

nom(sem:N, type:T,con:G) -->
	adj(sem:A,type:T1,con:G1,t:qual),
	nom(sem:N1,type:T2,con:G2),
	{logform(A,N1,N),% lambda application
	 f_app(G1,T1,G2,T2,G,T)}.% type construction

%%%%%%%%%%%%%%%%%
% B. Categories:
%%%%%%%%%%%%%%%%%

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

conj(sem:C,type:T,con:G,t:em) -->
	[], {conj2(C,T,G)}.

conj(sem:C,type:T,con:G,t:full) -->
	[X], {conj1(C,T,G,X)}.

adj(sem:A,type:T,con:G,t:Y) -->
	[X],{adj(A,T,G,Y,X)}.

neg(sem:N,type:T,con:G) -->
	[X], {neg(N,T,G,X)}.









