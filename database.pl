%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      %
%       CONTENT LEXICON FOR            %
%	    LITE ENGLISH               %
%	     & QUERIES                 %
%                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Lexicon:


% Common nouns


% noun(-meaning, -type, -token)


noun(lam(X,name(X)),imp(e,t),[],name).
noun(lam(X,height(X)),imp(e,t),[],height).

noun(lam(X,state(X)),imp(e,t),[],state).
noun(lam(X,state_abbreviation(X)),imp(e,t),[],state_abbraviation).
noun(lam(X,population(X)),imp(e,t),[],population).
noun(lam(X,area(X)),imp(e,t),[],area).

noun(lam(X,number(X)),imp(e,t),[],number).

noun(lam(X,highest_point(X)),imp(e,t),[],highest_point).
noun(lam(X,highest_elevation(X)),imp(e,t),[],highest_elevation).
noun(lam(X,lowest_point(X)),imp(e,t),[],lowest_point).
noun(lam(X,lowest_elevation(X)),imp(e,t),[],lowest_elevation).


% Proper nouns


% pnoun(-meaning, -type, -position, -assertion, -token)


pnoun(lam(P,app(P,john)),imp(imp(e,t),t),[],_,abox,john).
pnoun(lam(P,app(P,mary)),imp(imp(e,t),t),[],_,abox,mary).
pnoun(lam(P,app(P,julian)),imp(imp(e,t),t),[],_,abox,julian).

pnoun(lam(P,app(P,john)),imp(imp(e,t),t),[],_,cq,john).
pnoun(lam(P,app(P,mary)),imp(imp(e,t),t),[],_,cq,mary).
pnoun(lam(P,app(P,julian)),imp(imp(e,t),t),[],_,cq,julian).



% Verbs


% v(-meaning, -type, -category, -assertion, -position, -polarity, -voice, -token) 


% (transitive, positive, active voice)


v(lam(Z,lam(X,app(Z,lam(Y,loves(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,plus,act,loves).
v(lam(Z,lam(X,app(Z,lam(Y,rules(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,plus,act,rules).
v(lam(Z,lam(X,app(Z,lam(Y,kills(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,plus,act,kills).


% (transitive, negative, active voice)


v(lam(Z,lam(X,app(Z,lam(Y,loves(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,neg,act,love).
v(lam(Z,lam(X,app(Z,lam(Y,rules(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,neg,act,rule).
v(lam(Z,lam(X,app(Z,lam(Y,kills(X,Y))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,neg,act,kill).


% (transitive, positive, passive voice)


v(lam(Z,lam(X,app(Z,lam(Y,loves(Y,X))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,plus,pass,loved).
v(lam(Z,lam(X,app(Z,lam(Y,rules(Y,X))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,plus,pass,ruled).
v(lam(Z,lam(X,app(Z,lam(Y,kills(Y,X))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,plus,pass,killed).


% (transitive, negative, passive voice)


v(lam(Z,lam(X,app(Z,lam(Y,loves(Y,X))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,neg,pass,loved).
v(lam(Z,lam(X,app(Z,lam(Y,rules(Y,X))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,neg,pass,ruled).
v(lam(Z,lam(X,app(Z,lam(Y,kills(Y,X))))),imp(imp(imp(e,t),t),imp(e,t)),[],trans,_,_,neg,pass,killed).


% (intransitive, positive)


v(lam(X,rules(X)),imp(e,t),[],intrans,_,_,plus,act,rules).
v(lam(X,runs(X)),imp(e,t),[],intrans,_,_,plus,act,runs).


% (intransitive, negative)


v(lam(X,rules(X)),imp(e,t),[],intrans,_,_,neg,act,rule).
v(lam(X,runs(X)),imp(e,t),[],intrans,_,_,neg,act,run).


% Adjectives


% adj(-meaning, -type, -context, -token)


% (attributive)


adj(lam(X,old(X)),imp(e,t),[],att,old).
adj(lam(X,cunning(X)),imp(e,t),[],att,cunning).
adj(lam(X,human(X)),imp(e,t),[],att,human).


% (qualificative)


adj(lam(P,lam(X,and(old(X),app(P,X)))),imp(imp(e,t),imp(e,t)),[],qual,old).
adj(lam(P,lam(X,and(cunning(X),app(P,X)))),imp(imp(e,t),imp(e,t)),[],qual,cunning).





