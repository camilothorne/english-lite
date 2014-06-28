%%%%%%%%%%%%%%%%%%%%%
%
%    PARSER FOR
%   LITE ENGLISH
%
%%%%%%%%%%%%%%%%%%%%%

% Resources (grammar) and modules:

:- consult(lite_english).
:- use_module(readLine,[readLine/1]). %lexer
:- use_module(cq2tptp,[fol2tptp2/2]). %translator
:- use_module(fol2bliksem,[fol2bliksem/2]). %another one

% Calling the parser as an interactive NL interpreter
% (notice the recursive calls):

parses :- nl,
	 readLine(X),
	  ( X = [stop] ->    nl,
			 write('Bye!'),
			 !;
	                 ( s(sem:S,type:T,con:G,gap:_,ass:_,X,[]) ->
			   nl,
			   write('tokenized':X),
			   nl,
			   nl,
			   write('normalized meaning representation:'),
			   nl,
			   nl,
			   write('lambda':S),
			   nl,
			   open('/home/camilo/unison/prolog/dl_lite/spass_lite.txt',append,F),
			   fol2tptp2(S,F),close(F), %saves the MRs in a file
			   nl,
			   write('type':T),
			   nl,
			   nl,
			   write('context':G),
			   nl,
			   nl,
			   write('OK'),
			   nl,
			   parses; %(recursive call)
			   nl,
			   write('No parse!'),
			   nl,
			   parses  %(recursive call)
			 )
	 ).


% Calling the parser (notice the absence of
% recursive calls):

parsesimp :- 	nl,
	 	readLine(X),
		( s(sem:S,type:T,con:G,gap:_,ass:_,X,[]) ->
				   nl,
				   write('tokenized':X),
				   nl,
				   nl,
				   write('normalized meaning representation:'),
				   nl,
				   nl,
				   write('lambda':S),
				   nl,
				   open('/home/camilo/unison/prolog/dl_lite/spass_lite.txt',append,F),
				   fol2tptp2(S,F),close(F), %saves the MRs as Spass input formulas
				   open('/home/camilo/unison/prolog/dl_lite/blik_lite.txt',append,FF),
				   fol2bliksem(S,FF),close(FF), %saves the MRs as Bliksem input formulas
				   nl,
				   write('type':T),
				   nl,
				   nl,
				   write('context':G),
				   nl,
				   nl,
				   halt;
				   nl,
				   write('No parse!'),
				   nl,
				   halt
		 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarks:

%    preds_1 -> preds_2 ; preds_3 is equal to
%    if preds_1 then preds_2 else preds_3
%    i.e. a conditionsl control structure!

%    This program stores the LF in a file
%    if parse succeds. To close the output
%    stream, we have to type 'stop'
%    in the prompt.















