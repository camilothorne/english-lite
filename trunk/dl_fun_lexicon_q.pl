%%%%%%%%%%%%%%%%%%%%%
%			     
%  FUNCTION LEXICON FOR	       
%       QUERIES		       
%					
%%%%%%%%%%%%%%%%%%%%%




% Determiners (quantifiers)


% det(-meaning, -type, -conetxt, -position, -assertion, -token)


det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],_,_,some).
%det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],_,_,a).
%det(_,[],_,_,a).

det(lam(P,(lam(Q,some(X,and( app(P,X), app(Q,X) ))))),imp(imp(e,t),imp(imp(e,t),t)),[],_,_,somebody).


% Dummy category for relative clauses


% dummy(-meaning, -type, -context)


dummy(lam(P,app(P,T)),imp(imp(e,t),t),[T:e],_,_). % non-empty context!


% Pronouns


%pro(-meaning, -type, -context, -position, -assertion, -token)


pro(lam(P,some(X,app(P,X))),imp(imp(e,t),t),[],_,_,somebody).
pro(lam(P,some(X,app(P,X))),imp(imp(e,t),t),[],_,_,someone).
pro(lam(P,some(X,app(P,X))),imp(imp(e,t),t),[],_,_,something).
pro(lam(P,some(X,app(P,X))),imp(imp(e,t),imp(e,t)),[],_,_,somebody).


% Relative pronouns


% relp(-meaning, type, -context, -token)


relp(lam(Q,lam(P,lam(Y,and(app(P,Y),app(Q,Y))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],b,who).
relp(lam(Q,lam(P,lam(Y,and(app(P,Y),app(Q,Y))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],b,that).

relp(lam(P,lam(X,app(P,X))),imp(imp(e,t),imp(e,t)),[],d,that).
relp(lam(P,lam(X,app(P,X))),imp(imp(e,t),imp(e,t)),[],d,who).

relp(lam(Q,lam(X,lam(P,app(X,lam(Y,and(app(P,Y),app(Q,Y))))))),imp(imp(e,t),imp(imp(imp(e,t),t),imp(imp(e,t),t))),[],a,who).
relp(lam(Q,lam(X,lam(P,app(X,lam(Y,and(app(P,Y),app(Q,Y))))))),imp(imp(e,t),imp(imp(imp(e,t),t),imp(imp(e,t),t))),[],a,that).


% Interrogative pronouns


relp(lam(P,lam(Q,lam(X,and(app(P,X),app(Q,X))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],c,which).
relp(lam(P,lam(X,app(P,X))),imp(imp(e,t),imp(e,t)),[],c,who).
relp(lam(P,lam(X,app(P,X))),imp(imp(e,t),imp(e,t)),[],c,which).


% Conjunction


% conj(-meaning, -type, -context, -token, -kind)


conj1(lam(P,lam(Q,lam(X,and(app(P,X),app(Q,X))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[],and).
conj2(lam(P,lam(Q,lam(X,and(app(P,X),app(Q,X))))),imp(imp(e,t),imp(imp(e,t),imp(e,t))),[]).


% Copula (identity)


% conj(-meaning, -type, -context, -token, -kind)


cop1(lam(Z,lam(X,app(Z,lam(Y,eq(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],is).
cop2(lam(Z,lam(X,app(Z,lam(Y,eq(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[]).















