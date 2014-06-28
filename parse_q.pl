%%%%%%%%%%%%%%
%
%
%  PARSER FOR THE
%  QUESTION FRAGMENT
%
%
%%%%%%%%%%%%%%



% Resources (grammar) and modules:


:- consult(questions).
:- use_module(readLine,[readLine/1]). %(lexer)
:- use_module(cq2tptp,[fol2tptp/2]). %translator
%:- use_module(fol2bliksem,[fol2bliksem/2]). %another one


% Calling the parser:


parseq:-
	 readLinemine(X),
	(q(sem:[lf:Q,type:T,con:G],_,X,[]) ->
			   nl,
			   write('tokenized':X),
			   nl,
			   write('MR':Q),
			   nl,
			   open('/home/camilo/unison/prolog/dl_lite/spass.txt',append,F),
			   fol2tptp(Q,F),close(F), %saves the MRs in a file
			   write('type':T),
			   nl,
			   write('context':G),
			   nl,nl;
			   		nl,
					write('No parse!'), 
					nl,nl
	).


%%%%%%%%%%%%%%%

readLinemine([who,loves,julian]).

% Remarks:


%    preds_1 -> preds_2 ; preds_3 is equal to
%    if preds_1 then preds_2 else preds_3
%    i.e. a conditional control structure!

%    This program stores the LF in a file
%    if parse succeds. To close the output
%    stream, we have to type 'stop'
%    in the prompt.






