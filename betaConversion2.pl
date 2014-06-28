/*************************************************************************

                          BETA CONVERSION 2                    
 

*************************************************************************/


:- module(betaConversion2,[infix/0,
                           prefix/0,
			   betaConvert/2]).


:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                compose/3]).


:- use_module(alphaConversion,[alphaConvert/2,
                               alphabeticVariants/2]).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/


betaConvert(X,Y):-
   betaConvert(X,Y,[]).


/*========================================================================
   Beta-Conversion (comment-in for tracing)
========================================================================*/

/*
betaConvert(X,_,S):-
   nl, write(expre), tab(1), print(X),
   nl, write(stack), tab(1), print(S),
   fail.
*/

/*========================================================================
   Beta-Conversion (core stuff)
========================================================================*/


betaConvert(X,Y,[]):-
   var(X),
   Y=X.

betaConvert(Expression,Result,Stack):- 
   nonvar(Expression),
   Expression = app(Functor,Argument),
   nonvar(Functor),
   alphaConvert(Functor,Converted),                                   
%  to suppress alpha-conversion
%  comment-out the latter line and
%  comment-in this line:
%  Functor=Converted,  
   betaConvert(Converted,Result,[Argument|Stack]).

betaConvert(Expression,Result,[X|Stack]):-
   nonvar(Expression),
   Expression = lam(X,Formula),
   betaConvert(Formula,Result,Stack).

betaConvert(Formula,Result,[]):-
   nonvar(Formula),
   \+ (Formula = app(X,_), nonvar(X)),
   compose(Formula,Functor,Formulas),
   betaConvertList(Formulas,ResultFormulas),
   compose(Result,Functor,ResultFormulas).


/*========================================================================
   Added by me: extension of Beta-Conversion
=========================================================================*/


% I add a second halting condition for the loop!. If we have
% an expression of the form (a)E, where a is a constant and
% E an arbitrary expression, the algorithm halts, e.g.:
%      (a)b -> (a)b
%      (a)x -> (a)x.
% B&B's definition provide that the following
% reductions hold:
%      (X)Y -> (X)Y
%      (X)X -> (X)X.
% Note that this implementation doesn't follow
% exactly the definition of Beta-Conversion.
% However, the algorithm is sound and complete
% w.r.t. Beta-Conversion. The fact that it always halts
% comes from the fact that we only use 
% strongly normalizable lambda expressions!



betaConvert(Expression,Expression,[]):-
	nonvar(Expression),
	Expression = app(F,_),
	atom(F).



/*========================================================================
   Beta-Convert a list
========================================================================*/


betaConvertList([],[]).

betaConvertList([Formula|Others],[Result|ResultOthers]):-
   betaConvert(Formula,Result),
   betaConvertList(Others,ResultOthers).
