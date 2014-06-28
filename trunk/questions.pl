/******************************
*******************************


  SIMPLE CONJUNCTIVE QUERIES


*******************************
******************************/



:- consult(dl_fun_lexicon_q).
:- consult(dl_con_lexicon_q).


:- use_module(sem_dl,[logform/3]).
:- use_module(betaConversion2,[infix/0]).
:- use_module(lambda_abs,[abs_f/2,
			  f_app/6,
			  f_abs/4]).



/***********************************************************
 
                      NOTE 

************************************************************


We recall the signature of the semantic functions/relations:


1. logform(-exp1,-exp2,+exp3) :


(computes the beta reduct exp3 of exp1 applied to exp2)


2. abs_f(-exp,+lam(var,exp)):


(computes the immediate lambda abstraction)


3. f_abs(-con1,-type1,+con2,+imp(type2,type1)):


(a.k.a. --> introduction)
(type2 is in con2)


4. f_app(-con1,-imp(type1,type2),-con2,-type1,+con3,+type2):


(a.k.a. --> elimination)



************************************************************/



% A. Phrase structure rules:



/************************************************************/


% 1. Wh-Questions:



% (No verb inversion!)



q(sem:[lf:Q,type:T,con:G],syn:[class:wh]) --> 
	wh(sem:[lf:P,type:T1,con:G1]),
	s(sem:[lf:S,type:T2,con:G2],syn:[gap:yes,ass:cq]),
	{abs_f(S,SS),
	 f_abs(G2,T2,GG,TT),
	 logform(P,SS,Q),
	 f_app(G1,T1,GG,TT,G,T),
	 infix}.



/***********************************************************/



% 2. Y/N-Questions:



% (The verb gets inverted!)



q(sem:[lf:Q,type:T,con:G],syn:[class:yn]) --> 
	[is],
	np(sem:[lf:NP,type:T1,con:G1],syn:[pos:subj,gap:no,ass:cq,coord:no]),
	vp(sem:[lf:VP,type:T2,con:G2],syn:[pos:pred,ass:cq,pol:_,voice:_]),	
	{logform(NP,VP,Q), 
	 f_app(G1,T1,G2,T2,G,T),
	 infix}. 


q(sem:[lf:Q,type:T,con:G],syn:[class:yn]) --> 
	[does],
	np(sem:[lf:NP,type:T1,con:G1],syn:[pos:subj,gap:no,ass:cq,coord:no]),
	vp(sem:[lf:VP,type:T2,con:G2],syn:[pos:pred,ass:cq,pol:neg,voice:_]),	
	{logform(NP,VP,Q), 
	 f_app(G1,T1,G2,T2,G,T),
	 infix}. 



/*************************************************************/



% 3. Sentences:



% (subordinated clause or sentences with gaps for Wh-questions)



s(sem:[lf:S,type:T,con:G],syn:[gap:yes,ass:cq])  --> 
	np(sem:[lf:NP,type:T1,con:G1],syn:[pos:subj,gap:yes,ass:cq,coord:no]),
	vp(sem:[lf:VP,type:T2,con:G2],syn:[pos:pred,ass:cq,pol:plus,voice:_]),		   
	{logform(NP,VP,S), 
	 f_app(G1,T1,G2,T2,G,T)}. 



/************************************************************/



% 4. Wh operators:



wh(sem:[lf:W,type:T,con:G]) --> 
	relp(sem:[lf:WW,type:TT,con:GG],syn:[t:c]), % who
	{W = WW,
	 T = TT,
	 G = GG}.


wh(sem:[lf:W,type:T,con:G]) --> 
	relp(sem:[lf:P,type:T1,con:G1],syn:[t:c]), % which
	noun(sem:[lf:N,type:TN,con:GN]),
	{logform(P,N,W),
	 f_app(G1,T1,GN,TN,G,T)}.



/***********************************************************/



% 4. Noun phrases:



np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) --> 
	pnoun(sem:[lf:PN,type:T1,con:G1],syn:[pos:X,ass:Y]),
	{NP = PN,
	 T = T1,
	 G = G1}.


np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) --> 
	det(sem:[lf:Det,type:T1,con:G1],syn:[pos:X,ass:Y]),
	noun(sem:[lf:N,type:T2,con:G2]),				    
	{logform(Det,N,NP),
	 f_app(G1,T1,G2,T2,G,T)}.


np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) -->
	pro(sem:[lf:Pro,type:T1,con:G1],syn:[pos:X,ass:Y]),                            
	{NP = Pro,
	 T = T1,
	 G = G1}.


% for the following two rules we type-raise the relative
% (we type-raise relatives of kind c:
%  it stands for a whole NP this time)!


np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) -->
	pro(sem:[lf:Pro,type:TD,con:GD],syn:[pos:X,ass:Y]),
	relp(sem:[lf:P,type:TP,con:GP],syn:[t:a]),% attention here!
	s(sem:[lf:S,type:T2,con:G2],syn:[gap:yes,ass:cq]),
	{abs_f(S,SS),
	 f_abs(G2,T2,GG,TT),
	 logform(P,SS,PP),
	 f_app(GP,TP,GG,TT,GC,TC),
	 logform(PP,Pro,NP),
	 f_app(GC,TC,GD,TD,G,T)}.


% for this one too!


np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) -->
	pnoun(sem:[lf:PN,type:TD,con:GD],syn:[pos:X,ass:Y]),
	[,],
	relp(sem:[lf:P,type:TP,con:GP],syn:[t:a]),% attention here!
	s(sem:[lf:S,type:T2,con:G2],syn:[gap:yes,ass:cq]),
	{abs_f(S,SS),
	 f_abs(G2,T2,GG,TT),
	 logform(P,SS,PP),
	 f_app(GP,TP,GG,TT,GC,TC),
	 logform(PP,PN,NP),
	 f_app(GC,TC,GD,TD,G,T)}.


np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) -->
	det(sem:[lf:Det,type:TD,con:GD],syn:[pos:X,ass:Y]),
	noun(sem:[lf:N,type:TN,con:GN]),
	relp(sem:[lf:P,type:TP,con:GP],syn:[t:b]),
	s(sem:[lf:S,type:T2,con:G2],syn:[gap:yes,ass:cq]),
	{abs_f(S,SS),
	 f_abs(G2,T2,GG,TT),
	 logform(P,SS,PP),
	 f_app(GP,TP,GG,TT,GGG,TTT),
	 logform(PP,N,NN),
	 f_app(GGG,TTT,GN,TN,GC,TC),
	 logform(Det,NN,NP),
	 f_app(GD,TD,GC,TC,G,T)}.


np(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:yes,ass:Y,coord:no]) -->
	dummy(sem:[lf:Pro,type:T1,con:G1],syn:[pos:X,ass:Y]),                            
	{NP = Pro,
	 T = T1,
	 G = G1}.



/***********************************************************/



% 5. Verb phrases:



/*********************************************************/



% A. Positive polarity:



/*********************************************************/



% a. Simple:



vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:plus,voice:act]) --> 
	cop(sem:[lf:V,type:T1,con:G1],syn:[t:full]),     
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:plus,voice:act]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:X,ass:Y,pol:plus,voice:act]),     
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:plus,voice:pass]) -->
	[is],
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:X,ass:Y,pol:plus,voice:pass]),
	[by],
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:plus,voice:act]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:intrans,pos:X,ass:Y,pol:plus,voice:act]),                    
	{VP = V,
	 T = T1,
	 G = G1}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:_,ass:_,pol:plus,voice:act]) -->
	[is,a],                   
	noun(sem:[lf:N,type:T1,con:G1]),		 
	{VP = N,
	 T = T1,
	 G = G1}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:_,ass:_,pol:plus,voice:_]) --> 
	[is],                   
	adj(sem:[lf:A,type:T1,con:G1],syn:[t:att]),				
	{VP = A,
	 T = T1,
	 G = G1}.



% b. coordinated:



vp(sem:[lf:VP,type:T,con:G],syn:[pos:Y,ass:X,pol:plus,voice:_]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:intrans,pos:Y,ass:X,pol:plus,voice:act]),       
	conj(sem:[lf:CF,type:T2,con:G2],syn:[t:full]),       
	vp(sem:[lf:VP1,type:T3,con:G3],syn:[pos:Y,ass:X,pol:plus,voice:_]),       
	{logform(CF,VP1,L),
	 f_app(G2,T2,G3,T3,GG,TT),
	 logform(L,V,VP),
	 f_app(GG,TT,G1,T1,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:pred,ass:X,pol:plus,voice:_]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:pred,ass:X,pol:plus,voice:act]), 
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:pred,gap:no,ass:X,coord:no]),
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),       
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:pred,ass:tbox,pol:plus,voice:_]),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:pred,ass:X,pol:plus,voice:_]) --> 
	[is],
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:X,ass:Y,pol:neg,voice:pass]),
	[by],
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),	 
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:pred,ass:X,pol:plus,voice:_]),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:Y,ass:X,pol:plus,voice:_]) --> 
	[is,a],                   
	noun(sem:[lf:N,type:T1,con:G1]),	
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:Y,ass:X,pol:plus,voice:_]),
	{logform(CF,VP1,L2),
	 logform(L2,N,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:Y,ass:X,pol:plus,voice:_]) -->
	[is],
	adj(sem:[lf:A,type:T1,con:G1],syn:[t:att]),	 
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:Y,ass:X,pol:plus,voice:_]),
	{logform(CF,VP1,L2),
	 logform(L2,A,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.



/***************************************************************/



% B. Negative polarity:



/***************************************************************/



% a. Simple:



vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:neg,voice:act]) --> 
	cop(sem:[lf:V,type:T1,con:G1],syn:[t:em]),     
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:neg,voice:act]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:X,ass:Y,pol:neg,voice:act]),     
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:neg,voice:pass]) -->
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:X,ass:Y,pol:neg,voice:pass]),
	[by],
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:X,ass:Y,pol:neg,voice:act]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:intrans,pos:X,ass:Y,pol:neg,voice:act]),                    
	{VP = V,
	 T = T1,
	 G = G1}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:_,ass:_,pol:neg,voice:act]) -->
	[a],                   
	noun(sem:[lf:N,type:T1,con:G1]),		 
	{VP = N,
	 T = T1,
	 G = G1}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:_,ass:_,pol:neg,voice:_]) -->                  
	adj(sem:[lf:A,type:T1,con:G1],syn:[t:att]),				
	{VP = A,
	 T = T1,
	 G = G1}.



% b. Coordinated:



vp(sem:[lf:VP,type:T,con:G],[syn:pos:Y,ass:X,pol:neg,voice:_]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:intrans,pos:Y,ass:X,pol:neg,voice:act]),       
	conj(sem:[lf:CF,type:T2,con:G2],syn:[t:full]),       
	vp(sem:[VP1,type:T3,con:G3],syn:[pos:Y,ass:X,pol:plus,voice:_]),       
	{logform(CF,VP1,L),
	 f_app(G2,T2,G3,T3,GG,TT),
	 logform(L,V,VP),
	 f_app(GG,TT,G1,T1,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:pred,ass:X,pol:neg,voice:_]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:pred,ass:X,pol:neg,voice:act]), 
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:pred,gap:no,ass:X,coord:no]),
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),       
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:pred,ass:tbox,pol:plus,voice:_]),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:pred,ass:X,pol:neg,voice:_]) --> 
	v(sem:[lf:V,type:T1,con:G1],syn:[cat:trans,pos:X,ass:Y,pol:neg,voice:pass]),
	[by],
	np(sem:[lf:NP,type:T2,con:G2],syn:[pos:X,gap:no,ass:Y,coord:_]),	 
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:pred,ass:X,pol:plus,voice:_]),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:Y,ass:X,pol:neg,voice:_]) --> 
	[a],                   
	noun(sem:[lf:N,type:T1,con:G1]),	
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:Y,ass:X,pol:_,voice:_]),
	{logform(CF,VP1,L2),
	 logform(L2,N,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.


vp(sem:[lf:VP,type:T,con:G],syn:[pos:Y,ass:X,pol:neg,voice:_]) --> 
	adj(sem:[lf:A,type:T1,con:G1],syn:[t:att]),	 
	conj(sem:[lf:CF,type:T3,con:G3],syn:[t:full]),
	vp(sem:[lf:VP1,type:T4,con:G4],syn:[pos:Y,ass:X,pol:_,voice:_]),
	{logform(CF,VP1,L2),
	 logform(L2,A,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(GG,TT,G1,T1,G,T)}.



/*********************************************************/



% 5. Nominals:



noun(sem:[lf:N,type:T,con:G]) -->
	nounc(sem:[lf:NN,type:TT,con:GG]),
	{N = NN,
	 T = TT,
	 G = GG}.


noun(sem:[lf:N,type:T,con:G]) --> 
	adj(sem:[lf:A,type:T1,con:G1],syn:[t:qual]), 
	noun(sem:[lf:N1,type:T2,con:G2]),
	{logform(A,N1,N),
	 f_app(G1,T1,G2,T2,G,T)}.


noun(sem:[lf:NP,type:T,con:G],syn:[pos:X,gap:no,ass:Y,coord:no]) -->
	nounc(sem:[lf:PN,type:TD,con:GD],syn:[pos:X,ass:Y]),
	relp(sem:[lf:P,type:TP,con:GP],syn:[t:b]),% attention here!
	s(sem:[lf:S,type:T2,con:G2],syn:[gap:yes,ass:cq]),
	{abs_f(S,SS),
	 f_abs(G2,T2,GG,TT),
	 logform(P,SS,PP),
	 f_app(GP,TP,GG,TT,GC,TC),
	 logform(PP,PN,NP),
	 f_app(GC,TC,GD,TD,G,T)}.



/********************************************************/



% B. Categories:



/*******************************************************/



nounc(sem:[lf:N,type:T,con:G]) --> 
	[X], {nounc(N,T,G,X)}.

cop(sem:[lf:E,type:T,con:G],syn:[t:full]) -->
	[X], {cop1(E,T,G,X)}.

cop(sem:[lf:E,type:T,con:G],syn:[t:em]) -->
	[], {cop2(E,T,G)}.

det(sem:[lf:Det,type:T,con:G],syn:[pos:Pos,ass:Ass]) --> 
	[X], {det(Det,T,G,Pos,Ass,X)}.

pro(sem:[lf:Pro,type:T,con:G],syn:[pos:Pos,ass:Ass]) --> 
	[X], {pro(Pro,T,G,Pos,Ass,X)}.

pnoun(sem:[lf:PN,type:T,con:G],syn:[pos:Pos,ass:Ass]) --> 
	[X], {pnoun(PN,T,G,Pos,Ass,X)}.

v(sem:[lf:S,type:T,con:G],syn:[cat:Cat,pos:Pos,ass:Ass,pol:Pol,voice:V]) --> 
	[X], {v(S,T,G,Cat,Pos,Ass,Pol,V,X)}.

relp(sem:[lf:C,type:T,con:G],syn:[t:Y]) --> 
	[X],{relp(C,T,G,Y,X)}.

dummy(sem:[lf:D,type:T,con:G],syn:[pos:Pos,ass:Ass]) --> 
	[],{dummy(D,T,G,Pos,Ass)}.

conj(sem:[lf:C,type:T,con:G],syn:[t:full]) --> 
	[X], {conj1(C,T,G,X)}.

adj(sem:[lf:A,type:T,con:G],syn:[t:Y]) --> 
	[X],{adj(A,T,G,Y,X)}.

