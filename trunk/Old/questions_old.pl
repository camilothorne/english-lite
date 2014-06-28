
/*

  SIMPLE CONJUNCTIVE QUERIES


*/



:- consult(dl_fun_lexicon2).
:- consult(dl_con_lexicon2).

:- use_module(sem_dl,[logform/3]).
:- use_module(lambda_abs,[abs_f/2,
			  f_app/6,
			  f_abs/4]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% A. Phrase structure rules:



% 1. Wh-Questions:



% (No verb inversion!)



q(sem:Q,type:T,con:G,class:wh) --> 
	relp(sem:P,type:T1,con:G1,t:c),
	s(sem:S,type:T2,con:G2,gap:yes,ass:cq),
	[?],
	{abs_f(S,SS),
	 f_abs(G2,T2,GG,TT),
	 logform(P,SS,Q),
	 f_app(G1,T1,GG,TT,G,T)}.



% 2. Y/N-Questions:



% (The verb gets inverted!)



/*


q(sem:Q,type:T,con:G,class:yn) --> 
	s(sem:S,type:T1,con:G1,gap:no,ass:_), 
	[?],
	{Q = S,
	 T = T1,
	 G = G1}.


*/



q(sem:Q,type:T,con:G,class:yn) --> 
	[is],
	np(sem:NP,type:T1,con:G1,pos:subj,gap:no,ass:X,coord:no),
	vp(sem:VP,type:T2,con:G2,pos:pred,ass:X,pol:neg,voice:_),	
	[?],
	{logform(NP,VP,Q)}, 
	{f_app(G1,T1,G2,T2,G,T)}. 


q(sem:Q,type:T,con:G,class:yn) --> 
	[does],
	np(sem:NP,type:T1,con:G1,pos:subj,gap:no,ass:X,coord:no),
	vp(sem:VP,type:T2,con:G2,pos:pred,ass:X,pol:neg,voice:_),	
	[?],
	{logform(NP,VP,Q)}, 
	{f_app(G1,T1,G2,T2,G,T)}. 



/*


% 3. Sentences (with and without gaps):


s(sem:S,type:T,con:G,gap:Y,ass:cq)  --> 
	np(sem:NP,type:T1,con:G1,pos:subj,gap:Y,ass:X,coord:no),
	vp(sem:VP,type:T2,con:G2,pos:pred,ass:X,pol:plus,voice:_),		   
	{logform(NP,VP,S)}, 
	{f_app(G1,T1,G2,T2,G,T)}. 


*/



% 3. Sentences (subordinated or with gaps for Wh-questions):



s(sem:S,type:T,con:G,gap:yes,ass:cq)  --> 
	np(sem:NP,type:T1,con:G1,pos:subj,gap:yes,ass:X,coord:no),
	vp(sem:VP,type:T2,con:G2,pos:pred,ass:X,pol:plus,voice:_),		   
	{logform(NP,VP,S)}, 
	{f_app(G1,T1,G2,T2,G,T)}. 




% 4. Noun phrases:



np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) --> 
	pnoun(sem:PN,type:T1,con:G1,pos:X,ass:Y),
	{NP = PN,
	 T = T1,
	 G = G1}.


np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) --> 
	det(sem:Det,type:T1,con:G1,pos:X,ass:Y),
        noun(sem:N,type:T2,con:G2),				    
	{logform(Det,N,NP)},
	{f_app(G1,T1,G2,T2,G,T)}.


np(sem:NP,type:T,con:G,pos:X,gap:no,ass:Y,coord:no) -->
	pro(sem:Pro,type:T1,con:G1,pos:X,ass:Y),                            
	{NP = Pro,
	 T = T1,
	 G = G1}.


np(sem:NP,type:T,con:G,pos:X,gap:yes,ass:Y,coord:no) -->
	dummy(sem:Pro,type:T1,con:G1,pos:X,ass:Y),                            
	{NP = Pro,
	 T = T1,
	 G = G1}.




% 5. Verb phrases:


% a. Positive polarity:


% Simple:



vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:plus,voice:act) --> 
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:plus,voice:act),     
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),			           
	{logform(V,NP,VP),
	f_app(G1,T1,G2,T2,G,T)}.


vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:plus,voice:pass) -->
	[is],
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:plus,voice:pass),
	[by],
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:plus,voice:act) --> 
	v(sem:V,type:T1,con:G1,cat:intrans,pos:X,ass:Y,pol:plus,voice:act),                    
	{VP = V,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:_,ass:_,pol:plus,voice:act) -->
	[is,a],                   
	noun(sem:N,type:T1,con:G1),		 
	{VP = N,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:_,ass:_,pol:plus,voice:_) --> 
	[is],                   
	adj(sem:A,type:T1,con:G1,t:att),				
	{VP = A,
	 T = T1,
	 G = G1}.



% Coordinated:



vp(sem:VP,type:T,con:G,pos:Y,ass:X,pol:plus,voice:_) --> 
	v(sem:V,type:T1,con:G1,cat:intrans,pos:Y,ass:X,pol:plus,voice:act),       
	conj(sem:CF,type:T2,con:G2,t:full),       
	vp(sem:VP1,type:T3,con:G3,pos:Y,ass:X,pol:plus,voice:_),       
	{logform(CF,VP1,L),
	 f_app(G2,T2,G3,T3,GG,TT),
	 logform(L,V,VP),
	 f_app(GG,TT,G1,T1,G,T)
	}.



vp(sem:VP,type:T,con:G,pos:pred,ass:X,pol:plus,voice:_) --> 
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:X,pol:plus,voice:act), 
	np(sem:NP,type:T2,con:G2,pos:pred,gap:no,ass:X,coord:no),
	conj(sem:CF,type:T3,con:G3,t:full),       
	vp(sem:VP1,type:T4,con:G4,pos:pred,ass:tbox,pol:plus,voice:_),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.

/*


vp(sem:VP,type:T,con:G,pos:pred,ass:X,pol:plus,voice:_) --> 
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:X,pol:plus,voice:act),        
	np(sem:NP,type:T2,con:G2,pos:pred,gap:no,ass:X,coord:no),  
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP1,type:T4,con:G4,pos:pred,ass:X,pol:plus,voice:_),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.


*/



% a. Positive polarity:



% Simple:



vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:neg,voice:act) --> 
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:neg,voice:act),     
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),			           
	{logform(V,NP,VP),
	f_app(G1,T1,G2,T2,G,T)}.


vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:neg,voice:pass) -->
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:neg,voice:pass),
	[by],
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),			           
	{logform(V,NP,VP),
	 f_app(G1,T1,G2,T2,G,T)}.


vp(sem:VP,type:T,con:G,pos:X,ass:Y,pol:neg,voice:act) --> 
	v(sem:V,type:T1,con:G1,cat:intrans,pos:X,ass:Y,pol:neg,voice:act),                    
	{VP = V,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:_,ass:_,pol:neg,voice:act) -->
	[a],                   
	noun(sem:N,type:T1,con:G1),		 
	{VP = N,
	 T = T1,
	 G = G1}.


vp(sem:VP,type:T,con:G,pos:_,ass:_,pol:neg,voice:_) -->                  
	adj(sem:A,type:T1,con:G1,t:att),				
	{VP = A,
	 T = T1,
	 G = G1}.



% Coordinated:



vp(sem:VP,type:T,con:G,pos:Y,ass:X,pol:neg,voice:_) --> 
	v(sem:V,type:T1,con:G1,cat:intrans,pos:Y,ass:X,pol:neg,voice:act),       
	conj(sem:CF,type:T2,con:G2,t:full),       
	vp(sem:VP1,type:T3,con:G3,pos:Y,ass:X,pol:neg,voice:_),       
	{logform(CF,VP1,L),
	 f_app(G2,T2,G3,T3,GG,TT),
	 logform(L,V,VP),
	 f_app(GG,TT,G1,T1,G,T)
	}.



vp(sem:VP,type:T,con:G,pos:pred,ass:X,pol:neg,voice:_) --> 
	v(sem:V,type:T1,con:G1,cat:trans,pos:pred,ass:X,pol:neg,voice:act), 
	np(sem:NP,type:T2,con:G2,pos:pred,gap:no,ass:X,coord:no),
	conj(sem:CF,type:T3,con:G3,t:full),       
	vp(sem:VP1,type:T4,con:G4,pos:pred,ass:tbox,pol:neg,voice:_),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.



vp(sem:VP,type:T,con:G,pos:pred,ass:X,pol:neg,voice:_) --> 
	v(sem:V,type:T1,con:G1,cat:trans,pos:X,ass:Y,pol:neg,voice:pass),
	[by],
	np(sem:NP,type:T2,con:G2,pos:X,gap:no,ass:Y,coord:_),	 
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP1,type:T4,con:G4,pos:pred,ass:X,pol:neg,voice:_),
	{logform(CF,VP1,L2),
	 logform(V,NP,L1),
	 logform(L2,L1,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,G2,T2,GGG,TTT),
	 f_app(GG,TT,GGG,TTT,G,T)}.



vp(sem:VP,type:T,con:G,pos:pred,ass:X,pol:neg,voice:_) --> 
	[a],                   
	noun(sem:N,type:T1,con:G1),	
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP1,type:T4,con:G4,pos:pred,ass:X,pol:neg,voice:_),
	{logform(CF,VP1,L2),
	 logform(L2,N,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.



vp(sem:VP,type:T,con:G,pos:pred,ass:X,pol:neg,voice:_) --> 
	adj(sem:A,type:T1,con:G1,t:att),	 
	conj(sem:CF,type:T3,con:G3,t:full),
	vp(sem:VP1,type:T4,con:G4,pos:pred,ass:X,pol:neg,voice:_),
	{logform(CF,VP1,L2),
	 logform(L2,A,VP),
	 f_app(G3,T3,G4,T4,GG,TT),
	 f_app(G1,T1,GG,TT,G,T)}.



% 5. Nouns with qualificative adjectives (modifiers):



noun(sem:N,type:T,con:G) --> 
	adj(sem:A,type:T1,con:G1,t:qual), 
	noun(sem:N1,type:T2,con:G2),
	{logform(A,N1,N),
	 f_app(G1,T1,G2,T2,G,T)}.



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
	[], {conj(C,T,G,_)}.

conj(sem:C,type:T,con:G,t:full) --> 
	[X], {conj(C,T,G,X)}.

adj(sem:A,type:T,con:G,t:Y) --> 
	[X],{adj(A,T,G,Y,X)}.

