% takes list of lists and flattens it
flatten([[H1|T1]|T],[H1|Z]) :- flatten([T1|T],Z).
flatten([[H1]|T],[H1|Z]) :- flatten(T,Z).
flatten([[X|T]],[X|T]).

% sum of all of the elements of a list
sums([],0) :- !.
sums([X],X) :- !.
sums([X|T],Z) :- sums(T,Z1), Z is X + Z1, !.

% calculates the largest sum of any sublist in a list
maxsum([X],Z) :- sums(X,Z), !.
maxsum([X|T],Z) :- sums(X,Z1), maxsum(T,Z2), (Z1 >= Z2, Z is Z1; Z is Z2), !.

% validate if this is a better solution and store it if so
valid(L) :- x(Z,S1), maxsum(L,S2), (S2 < S1; S1 == -1), retract(x(Z,S1)), asserta(x(L,S2)), !.

% use flatten to infer the possible sublists
gen(A,N,_) :- flatten(L,A), length(L,N), valid(L), fail.
gen(_,_,R) :- x(R,_).

% calcall(list_of_integers, number_of_partitions, result).
calcall(A,N,R) :- asserta(x([[-1]],-1)), gen(A,N,R), retract(x(_,_)).
