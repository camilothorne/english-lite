:- consult(dl_fun_lexicon2).
:- consult(dl_con_lexicon2).


:- use_module(sem_dl,[logform/3]).

:- use_module(lambda_abs,[abs_f/2,
			  f_app/6,
			  f_abs/4]).


vp(sem:[lf:VP,type:T,con:G],syn:[pos:pred,ass:_,pol:neg,voice:_]) --> 
	[is,a],                   
	noun(sem:[lf:N,type:T1,con:G1]),	
	{VP = N,
	 T = T1,
	 G = G1}.


noun(sem:[lf:N,type:T,con:G]) --> 
	[X], {noun(N,T,G,X)}. 

conj(sem:[lf:C,type:T,con:G],syn:[t:em]) --> 
	[], {conj(C,T,G,_)}.


conj(sem:[lf:C,type:T,con:G],syn:[t:f]) --> 
	[X], {conj(C,T,G,X)}.
