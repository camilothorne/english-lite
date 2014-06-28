
/****************************************************/
/*********** COURS DE PROLOG 2002-2003 **************/
/****************************************************/


% On a travaillé dans le dialecte SWI-PROLOG.



/****************************************************/
/*************** PROCEDURES PREDEFINIES *************/
/****************************************************/


% append(L,LL,LLL) - concatène 2 listes.
% length(L,N).
% integer(N).
% findall(X, P(X), L) - où P est 1 procédure 
%                       quelconque, X 1 entrée
%                       quelconque et L la liste
%                       des X vérifiant P.
% 0 is N mod K - dit si N est ou n'est pas 1 
%                multiple de K.


/***************************************************/
/******************* MODULE ************************/
/***************************************************/


:- module(lang,[car/2,
		cdr/2,
		der/2,
		nieme/3,
	        elem/2,
		conc/3,
		long/2,
		long1/2,
		long2/3,
		supp1/3,
		supp2/3,
		sousl/2,
		comp/2,
		miroir/2,
		miroir1/2,
		miroir2/3,
		sel/3,
		permut/2,
		compt/3,
		pair/1,
		impair/1,
		somme/2,
		max/3,
		maxli/2,
		list_ent1/2,
		list_ent2/3,
		entre/3,
		entrep/3,
		entreK/4,
		comp2/2,
		unir/3,
		fib/2,
		fib1/2,
		fib2/5,
		fact/2,
		exp/3,
		moyen/2,
		insert/4,
		insord/3,
		ord/1,
		fact1/2,
		fact2/4,
		fact_com/2,
		fact_aux/4,
		tri_bulles/2,
		min/3,
		minli/2,
		tri_ins/2,
		tri_insert/2,
		tri_ins1/3]).



/*****************************************************/
/********* PROCEDURE LIBRARY #1 (PROLOG) *************/
/*****************************************************/



/* 1. Tête */

/* t: #<list> --> #<symbol> */

/* m(i,o) */


car([X|_], X).


% Rem: point de clause inductive!


/*OK*/


/*****************************************************/


/* 2. Queue */

/* m(i,o) */

/* t: #<list> --> #<list> */


cdr([_|X], X).
cdr([_|Q], L):- cdr(Q, L).


/*OK*/


/*****************************************************/


/* 3. Dernier */

/* m(i,o) */

/* t: #<list> --> #<symbol> */


der([X], X).
der([_|Q], Y):- der(Q, Y).


% Rem: der = anticar 


/*OK*/


/*****************************************************/


/* 4. Nième */

/* m(i,i,o) */

/* t: #<symbol>,#<list> --> #<integer> */


nieme(X, L, 1):- car(L, X).
nieme(X, L, N):- cdr(L, LL) , 
		 nieme(X, LL, M) ,
		 N is M+1.


/*OK*/


/*****************************************************/


/* 5. Elément */

/* m(i, o) */

/* t: #<list> --> #<symbol> */


elem(L, X):- car(L, X).
elem([_|Q], X):- elem(Q, X).


/*OK*/


/*****************************************************/


/* 6. Concatenée */

/* m(i,i,o) */

/* t: #<list>,#<list> --> #<list> */


conc([ ], L, L).
conc([T|Q], L, [T|QQ]):- conc(Q, L, QQ).


/*OK - cf. Delahaye (en partie)*/


/******************************************************/


/* 7. Longueur */

/* m(i,o) */

/* t: #<list> --> #<integer> */


long([ ], 0).
long([_|Q], N):- long(Q, M) , N is M+1.


/*OK*/


/******************************************************/


/* 7.1. Itération */

/* idem sauf pour long2! */

/* idem sauf pour long2! */


long1(L, N):- long2(L, N, 0).
long2([ ], N, Acc):- Acc = N.
long2([_|Q], N, Acc):- P is Acc+1,
		       long2(Q, N, P).


/*OK*/


/*******************************************************/


/* 8. Suppression */

/* m(i,i,o) */

/* t: #<symbol>,#<list> --> #<list> */


supp1(_, [ ], [ ]).
supp1(X, [T|Q], [T|L]) :- not(X=T) ,
			  supp1(X, Q, L).
supp1(X, [T|Q], L):- X=T, supp1(X, Q, L).


% Rem: il manquait la dernière clause!


/*OK!!!*/


/*******************************************************/


/* 9. Suppression 2 (première occ) */

/* m(i,i,o) */

/* t: #<symbol>,#<list> --> #<list> */


supp2(_, [ ], [ ]).
supp2(X, [X|Q], Q).
supp2(X, [T|Q], [T|L]):- not(X=T) , 
		         supp2(X, Q, L).


/*OK(cf. cours)*/


/*******************************************************/


/* 10. Sous liste propre */

/* m(i,o) */

/* t: #<list> --> #<list> */


sousl([ ], [ ]).
sousl([X|_], [X]).
sousl([T|Q], L):- not(Q=[ ]), conc([T],Q, L).
sousl([_|Q], L):- sousl(Q, L).


% Rem: toutes sauf qu'en désordre! 


/*OK(+-)*/


/*******************************************************/


/* 11. Compacte */

/* m(i,o) */

/* t: #<list> --> #<list> */


comp([ ], [ ]).
comp([X|Q], [X|L]):- not(elem(Q, X)), comp(Q, L).
comp([X|Q], L):- elem(Q, X), comp(Q, L).


/*OK!!!*/


/********************************************************/


/* 12. Miroir */

/* m(i,o) */

/* t: #<list> --> #<list> */


miroir([ ], [ ]).
miroir([T|Q], L):- conc(LL, [T], L) , 
		   miroir(Q, LL).


/*OK*/


/*********************************************************/


/* 12.1. Itération: */

/* idem */

/* idem sauf pour miroir2! */


miroir1(L, LL):- miroir2(L, LL, [ ]).
miroir2([ ], LL, Acc):- Acc = LL.
miroir2([T|Q], LL, Acc):- P = [T|Acc] ,
	                  miroir2(Q, LL, P).


/*OK*/


/*********************************************************/


/* 13. Ensemble égal */


/* Rem: L et LL ont les mêmes éléments à 
l'ordre près */

/* m(i,o) */

/* t: #<list> --> #<list> */


sel(X, [X|Q], Q).
sel(X, [T|Q], [T|QQ]):- sel(X, Q, QQ).

permut([ ], [ ]).
permut(L, [T|Q]):- sel(T, L, LL), permut(LL, Q).


/* cf. Sterling & Shaphiro */

/*OK*/


/*********************************************************/
/****************** LIBRARY #2 (PROLOG) ******************/
/*********************************************************/


/* I.1. Compter */

/* m(i,o) */

/* t: #<symbol>,#<list> --> #<integer> */


compt(_, [ ], 0).
compt(X, [X|Q], N):- compt(X, Q, M), 
	             N is M+1.
compt(X, [T|Q], N):- not(X=T),	
	             compt(X, Q, N).

/*OK*/


/*********************************************************/


/* I.2. Entiers pairs (d'une liste) */


/*m(i, o)*/

/* t: #<list> --> #<list> */


/**************** I.2.1. Préliminaires: *******************/
/**********************************************************/


pair(N):- integer(N), 0 is N mod 2.

impair(N):- integer(N), not(pair(N)).

% O.K.


/**********************************************************/


/* I.3. Somme d'une liste d'entiers */

/* m(i,o) */

/* t: #<list> --> #<integer> */


somme([ ], 0).
somme([X], X).
somme([T|Q], N):- somme(Q, M), N is M+T.


/*OK*/


/**********************************************************/
/**********************************************************/


/* II.1. Elément maximal */

/* m(i,i,o) */

/* #<integer>, #<integer> --> #<integer> */


max(X, Y, X):- X >= Y.
max(X, Y, Y):- Y >= X.


/*OK*/


/***********************************************************/


/* II.2. Maximum (d'une liste) */

/* m(i,o) */

/* t: #<list> --> #<integer> */


maxli([X], X).
maxli([T|Q], N):- maxli(Q, M), max(T, M, N).


/*OK*/


/***********************************************************/
/***********************************************************/


/* III.1 Listes d'entiers */

/* m(i,o) */

/* t: #<integer> --> #<list> */

/* Itération! */


list_ent1(N, LN):- list_ent2(N, LN, [ ]).
list_ent2(0, LN, LN).
list_ent2(N, LM, Acc):-	P = [M|Acc],
	                list_ent2(M, LM, P),
			N is M+1.


/*OK*/
			

/************************************************************/


/* III.2. Un entier entre deux entiers */


/* m(i,i,o) */

/* t: #<integer>,#<integer> --> #<integer> */

entre(N, NN, N):- N=<NN.
entre(N, NN, M):- N<NN, P is N+1, 
		  entre(P, NN, M).



/*************************************************************/


/* III.3. Presque pareil, sauf que pair */


/* idem */

/* idem */


entrep(N, NN, M):- entre(N, NN, M), pair(M).



/*************************************************************/


/* III.4. Divisibilité modulo K */


/* m(i,o) */

/* t : #<integer>,#<integer>,
       #<integer> --> #<integer> */


entreK(N, NN, K, M):- entre(N, NN, M), 0 is M mod K.


/*************************************************************/
/********************** 3.FIBONACCI **************************/
/*************************************************************/


% Préliminaires: 


% I. Compacte.


comp2([ ], [ ]).
comp2([T|Q], X):- elem(Q,T),!,comp2(Q,X).
comp2([T|Q], [T|L]):- comp2(Q,L).


% cf. Vu en cours.


% II. Unir.


% t: #<list>, #<list> --> #<list>

% m(i,o)


unir(L, [ ], L).
unir([ ], L, L).
unir([X|Q], [Y|QQ], [X|QQQ]):- X>Y, unir([X|Q],QQ, QQQ).
unir([X|Q], [Y|QQ], [Y|QQQ]):- X<Y, unir(Q, [Y|QQ], QQQ).
unir([X|Q], [Y|QQ], [X, Y|QQQ]):-X=Y,!, unir(Q, QQ, QQQ).


% Je ne comprends toujours pas!


/*************************************************************/
/******************** LES ITERATIONS *************************/
/*************************************************************/


% III.1. La suite de fibonacci.


% Rem: double itération


% t: #<integer> --> #<integer>

% m(i,o)

% Rappel: u(0) = u(1) = 1.
%	  u(n+2) = u(n) + u(n+1).


/************************************************************/


% III.1.1. Def. récursive.


fib(0,1):-!.
fib(1,1):-!.
fib(N,NN):- Z is N-1, ZZ is N-2, 
	    fib(Z, M), fib(ZZ, MM), 
	    NN is M+MM.

% O.K.


/************************************************************/


% III.1.2. Def. itérative.


% Avec compteur:


fib1(0,1):-!.
fib1(0,1):-!.
fib1(N, NN):- fib2(N, NN, 0, 1, 1).
fib2(N, NN, N, NN, _):-!.
fib2(N, NN, C1, Acc1, Acc2):- C2 is C1+1, PP is Acc1+Acc2,
			      fib2(N, NN, C2, Acc2, PP).

% O.K.


/************************************************************/


% Remarques concernant les itérations.


% Les itérations visent à calculer une fonction via
% des valeurs (arguments) intermediares placés dans des
% registres (accumulateurs), jusqu'à ce que l'on arrive à  
% une certaine borne ou condition (elle boucle donc une
% nombre fini de fois jusqu'à la remplir).Elles se
% composent de i) une clause d'initialisation;
% ii) une clause de base ou arrêt (la condition à remplir);
% iii) une clause itérative comportant a) la définition
% des arguments intermédiares, b) la définition des 
% accumulateurs et c) l'itération proprement dite avec ses 
% termes mis de la façon la plus astucieuse! Eventuellement,
% on rajoute de compteurs qui bornent le calcul.


% Att: i)L'ordre des conjoints dans la dernière clause
%       est très importante, car elle relève de la syntaxe
%       Prolog il faut (dans les itérations), calculer pas
%       à pas les valeurs des variables de l'appel itératif.
%     ii)Le cut dans la clause de base évite que la procédure
%       refasse à nouveau le travail.


/*************************************************************/


% III.2. La fonction factorielle.

% III.2.1. Def. Itérative.

% Rem: idem

% t: idem

% m(i,o)

% Rappel: fact(0) = fact(1) = 1.
%         fact(n+1) = fact(n)*(n+1).


fact1(0, 1):-!.
fact1(N, NN):- fact2(N, NN, 1, 1).
fact2(1, NN, NN, _):-!.
fact2(N, NN, Acc1, Acc2):- M is N-1,PP is Acc2+1,
                           P is Acc1*PP,
			   fact2(M, NN, P, PP).
			  
% O.K.


/*************************************************************/


% Avec compteur:


fact_com(N, NN):- fact_aux(N, NN, 1, 1).
fact_aux(N, NN, NN, N):-!.
fact_aux(N, NN, Acc, C):- C1 is C+1, P is C1*Acc,
			  fact_aux(N, NN, P, C1).
	                  

% O.K.


/*************************************************************/


% III.2.2. Def récursive.

fact(0, 1):-!.
fact(1, 1):-!.
fact(N, NN):- M is N-1, fact(M, MM),
              NN is MM*N.



% Addendum.


exp(_,0,1):-!.
exp(N,1,N):-!.
exp(N,NN,NNN):- M is NN-1, exp(N,M,MM), NNN is MM*N.



% O.K.

% Rem: ils ne marchaient pas puisque le calcul du prédecesseur
%      (pour l'appel récursif) doit se faire en premier lieu!
%      Comme ça, en calculant f(n) on descend (on trouve f(n-1),
%      f(n-2), ..., f(0)) jusqu'à f(0) ou autrement jusqu'à
%      f(1) et, puis, on remonte!


/***********************************************************/
/********************  T.D.4. ******************************/
/***********************************************************/


% 0.Préliminaires.


% 1.Moyenne arithmétique.


moyen(L, N):- somme(L, M), long(L, MM), 
	      N is M/MM.


%O.K.


/**********************************************************/


% 2.Insertion.


insert(X, L, 0, [X|L]):-!.
insert(X, [T|L], N, [T|LL]):- M is N-1, 
	                      insert(X, L, M, LL).


%O.K.


/**********************************************************/


% 3.Insertion en ordre.
 

insord(X, [ ], [X]).
insord(X, [Y|Q], [X,Y|Q]):- X=<Y.
insord(X, [T|Q], [T|L]):- T<X, insord(X, Q, L).


%O.K.


/**********************************************************/


% 4.Listes ordonnées.


ord([ ]).
ord([_]).
ord([T|Q]):- Q=[TT|QQ], T<TT, ord([TT|QQ]).

%O.K.


/***********************************************************/
/******************* METHODES DE TRI ***********************/
/***********************************************************/


% 1. Tri à bulles (bubblesort).


tri_bulles(L, L):- ord(L),!.
tri_bulles([X, Y|Q], [Y, X|QQ]):- Y=<X, !, 
	                         tri_bulles([X|Q], [X|QQ]).
tri_bulles([T|Q], [T|QQ]):- tri_bulles(Q, QQ).



/**********************************************************/


% 2. Tri-insertion.


% 2.1. Minimum.


min(N, M, N):- N=<M.
min(N, M, M):- M=<N.


% O.K.


% 2.2. Minimum d'une liste.


minli([N], N).
minli([T|Q], N):- minli(Q, M), min(T, M, N).


% O.K.


/**********************************************************/


% Tri insertion:


% 2.3.1. Une définition possible.


tri_ins([ ], [ ]):-!.
tri_ins([T|Q], L):- tri_ins(Q, QQ), insord(T, QQ, L).


%O.K.


% 2.3.2. Une définition itérative.


tri_insert(L, LT):- tri_ins1(L, LT, [ ]).
tri_ins1([ ], LT, LT).
tri_ins1([T|L], LT, Acc):- insord(T, Acc, P),
	                   tri_ins1(L, LT, P).








