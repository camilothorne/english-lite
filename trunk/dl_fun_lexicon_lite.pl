%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FUNCTION LEXICON FOR
%    LITE ENGLISH
%
%%%%%%%%%%%%%%%%%%%%%%%%%%


% Determiners (quantifiers)


% det(-meaning, -type, -conetxt, -position, -assertion, -token)


det(lam(P,(lam(Q,all(X,imp( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],subj,tbox,every).
det(lam(P,(lam(Q,all(X,imp( app(P,X), not(app(Q,X)) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],subj,tbox,no).

%det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],pred,tbox,some).
%det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],_,_,some).
%det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],_,_,a).

%det(lam(P,(lam(Q,all(X,imp( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],subj,tbox,anybody).
%det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],pred,tbox,somebody).


% Dummy category for relative clauses


% dummy(-meaning, -type, -context)


%dummy(lam(P,app(P,T)),imp(imp(e,t),t),[T:e],_,_). % non-empty context!


% Pronouns


%pro(-meaning, -type, -context, -position, -assertion, -token)


pro(lam(P,(lam(Q,all(X,imp( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],subj,tbox,anybody).
pro(lam(P,(lam(Q,all(X,imp( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],subj,tbox,everybody).

% pro(lam(P,all(X,app(P,X))),imp(imp(e,t),t),[],subj,tbox,anybody).
% pro(lam(P,all(X,app(P,X))),imp(imp(e,t),t),[],subj,tbox,everybody).
%pro(lam(P,all(X,app(P,X))),imp(imp(e,t),t),[],subj,tbox,anything).

pro(lam(P,some(X,app(P,X))),imp(imp(e,t),t),[],_,tbox,somebody).
pro(lam(P,some(X,app(P,X))),imp(imp(e,t),t),[],_,tbox,someone).
%pro(lam(P,some(X,app(P,X))),imp(imp(e,t),t),[],_,tbox,something).


% Relative pronouns


% relp(-meaning, type, -context, -token)


% Role-inclusion


%relp(lam(Q,lam(X,lam(P,app(X,lam(Y,and(app(P,Y),app(Q,Y))))))),imp(imp(e,t),imp(imp(imp(e,t),t),imp(imp(e,t),t))),[],a,who).
%relp(lam(Q,lam(X,lam(P,app(X,lam(Y,and(app(P,Y),app(Q,Y))))))),imp(imp(e,t),imp(imp(imp(e,t),t),imp(imp(e,t),t))),[],a,that).


% Usual form


% relp(lam(Q,lam(P,lam(Y,and(app(P,Y),app(Q,Y))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],b,who).
% relp(lam(Q,lam(P,lam(Y,and(app(P,Y),app(Q,Y))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],b,that).


% Questions


% relp(lam(P,lam(X,app(P,X))),imp(imp(e,t),imp(e,t)),[],c,who).


% Conjunction


% conj(-meaning, -type, -context, -token, -kind)


conj1(lam(P,lam(Q,lam(X,and(app(P,X),app(Q,X))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],and).
conj2(lam(P,lam(X,app(P,X))),imp(imp(e,t),imp(e,t)),[]).
conj2(lam(P,lam(Q,lam(X,and(app(P,X),app(Q,X))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[]).



% Negation


% neg(-meaning, -type, -context, -token)


neg(lam(P,lam(X,not(app(P,X)))),imp(imp(e,t),imp(e,t)),[],not).


% Copula (identity)


% conj(-meaning, -type, -context, -token, -kind)


%cop1(lam(Z,lam(X,app(Z,lam(Y,eq(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],is).
%cop2(lam(Z,lam(X,app(Z,lam(Y,eq(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[]).
