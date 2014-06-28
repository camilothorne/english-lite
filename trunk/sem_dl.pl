%%%%%%%%%%%%%%%%%%%%%%%%
%                      %
%                      %
% Semantic procedures  %
%		       %
%                      %
%%%%%%%%%%%%%%%%%%%%%%%%



% Module:


:- module(seman_dl,[logform/3,
		    apply/3]).


% External modules:


:- use_module(betaConversion2,[betaConvert/2,
			      infix/0]).


% Procedures:


apply(X,Y,app(X,Y)).

logform(X,Y,Z) :- betaConvert(app(X,Y),Z).
