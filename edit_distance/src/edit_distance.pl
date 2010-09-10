% init the alphabet to use during edit distance calculation 
init(A,B) :- append(A,B,C), dedup(C,D), retractall(letters(_)), asserta(letters(D)).

dedup([],[]).
dedup([H|T],T2) :- member(H,T), dedup(T,T2).  dedup([H|T],[H|T2]) :- dedup(T,T2).

cs(H) :- letters(L), member(H,L).

% remove any element from the list
rem([X|R],X,R).
rem([F|R],X,[F|S]) :- rem(R,X,S).

% replace any given element in a list 
rep([X|R],Y,[Y|R]) :- X \= Y.
rep([X|R],Y,[X|S]) :- rep(R,Y,S).

% insert any given element into a list 
ins([X|R],Y,[X,Y|R]).
ins([X|R],Y,[Y,X|R]).
ins([X|R],Y,[X|S]) :- ins(R,Y,S).

% Substitution: A only has 1 element different from B
t1(A,B) :- cs(X), rep(A,X,B).

% Deletion: A is equal to B except for 1 additional element
t2(A,B) :- cs(X), rem(A,X,B).

% Insertion: B has one more element than A and all other elements are equal
t3(A,B) :- cs(X), ins(A,X,B).

% calculates the distance between two words in terms of characters
% that are different
diff([X|T1],[X|T2],N) :- diff(T1,T2,N), !.
diff([_|T1],[_|T2],N) :- diff(T1,T2,N2), N is N2 + 1, !.
diff(X,[],N) :- length(X,N), !.
diff([],Y,N) :- length(Y,N), !.

% convert [[a,b,c]] to [abc] for ease of reading
pp(A,B) :- findall(X,(member(D,A),atom_chars(X,D)),B).

% check if the current cost is higher than the best and fail if so
ccalc(AC,CC) :- CC is AC + 1, best(_,BC), CC < BC, !.

% are we moving closer to B or not ?
dcalc(A,B,X) :- diff(A,B,N1), diff(X,B,N2), N2 < N1, !.

% use t1, t2 and t3 operations to find a path between the words A and B
edist(A,B,[],C,C) :- diff(A,B,0).
edist(A,B,[X|R],AC,C) :- ccalc(AC,CC), (t1(A,X), dcalc(A,B,X), edist(X,B,R,CC,C);
                                        t2(A,X), dcalc(A,B,X), edist(X,B,R,CC,C);
                                        t3(A,X), dcalc(A,B,X), edist(X,B,R,CC,C)).

% is this result better than the last ? 
valid(B,R) :- best(B1,R1), R < R1, retractall(best(_,_)), asserta(best(B,R)), !.

% custom findall like rule
calcall(A,B,R) :- atom_chars(A,A1), atom_chars(B,B1), init(A1,B1), !, asserta(best([], 999999)), gen(A1,B1,R), retractall(best(_,_)).
gen(A,B,_) :- edist(A,B,R,0,C), valid(R,C), fail.
gen(_,_,(C,R1)) :- best(R,C), pp(R,R1).

% a few test scenarios
test(A,B) :- calcall(A,B,X), write(A), write(' -> '), write(X), write(' -> '), write(B), nl.

test :- test(xtesting,testing),
        test(aaaaaaa,zzzzzzz),
        test(abrackadbrea,abracadabra),
        test(shdup,'shut up'),
        test(dbag,douchebag).
