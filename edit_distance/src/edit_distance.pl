cs(H) :- atom_chars('abcdefghijklmnopqrstuvwyz',L), member(H,L).

% remove any element from the list
rem([X|R],X,R).
rem([F|R],X,[F|S]) :- rem(R,X,S).

% replace any given element in a list 
rep([X|R],Y,[Y|R]).
rep([X|R],Y,[X|S]) :- rep(R,Y,S).

% insert any given element into a list 
ins([X|R],Y,[Y,X|R]).
ins([X|R],Y,[X,Y|R]).
ins([X|R],Y,[X|S]) :- ins(R,Y,S).

% Substitution: A only has 1 element different from B
t1(A,B) :- cs(X), rep(A,X,B).

% Deletion: A is equal to B except for 1 additional element
t2(A,B) :- cs(X), rem(A,X,B).

% Insertion: B has one more element than A and all other elements are equal
t3(A,B) :- cs(X), ins(A,X,B).

x([],[],0) :- !.
x([H|T1],[H|T2],N) :- x(T1,T2,N), !.
x([H1|T1],[H2|T2],N) :- x(T1,T2,N2), N is N2 + 1, !.
x([H1|T1],[],N) :- x(T1,[],N2), N is N2 + 1, !.
x([],[H1|T1],N) :- x([],T1,N2), N is N2 + 1, !.

edist(A,B) :- atom_chars(A,A1), atom_chars(B,B1), edist(A1,B1,R), write(R), nl.

edist(A,B,[]) :- x(A,B,0).
edist(A,B,[X|R]) :- t1(A,X), x(A,B,N1), x(X,B,N2), N2 < N1, edist(X,B,R).
edist(A,B,[X|R]) :- t2(A,X), x(A,B,N1), x(X,B,N2), N2 < N1, edist(X,B,R).
edist(A,B,[X|R]) :- t3(A,X), x(A,B,N1), x(X,B,N2), N2 < N1, edist(X,B,R).
