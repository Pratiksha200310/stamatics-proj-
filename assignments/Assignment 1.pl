/* Membership operation */
member((Y,X), inverse(R)) :- member((X,Y), R).
member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

subset([], _).
subset([X|R], S) :- member(X, S), subset(R, S).

equal_sets(L1, L2) :- subset(L1, L2), subset(L2, L1).

delete_element(_, [], []).
delete_element(X, [X|R], Z) :- delete_element(X, R, Z).
delete_element(X, [Y|R], [Y|Z]) :- delete_element(X, R, Z).

remove_duplicates([], []).
remove_duplicates([X|R], [X|Z]) :- delete_element(X, R, L), remove_duplicates(L, Z).

/* Set union */
union([], S2, S2).
union([X|R], S2, [X|Z]) :- diff(S2, [X], S3), union(R, S3, Z).

append_lists([], L, L).
append_lists([X|R], L, [X|Z]) :- append_lists(R, L, Z).

map_cons(_, [], []).
map_cons(X, [Y|R], [[X|Y]|Z]) :- map_cons(X, R, Z).

/* Reflexive, symmetric, transitive closure membership check */
rst_closure_member((X,Y), Closure) :- member((X,Y), Closure), !.
rst_closure_member((X,X), _, Set) :- member(X, Set), !.
rst_closure_member((X,Y), Closure) :- member((Y,X), Closure), !.
rst_closure_member((X,Y), Closure) :- member((Z,X), Closure), diff(Closure, [(X,_)], NewClosure), rst_closure_member((Z,Y), NewClosure).
rst_closure_member((X,Y), Closure) :- member((Z,Y), Closure), diff(Closure, [(Y,_)], NewClosure), rst_closure_member((Z,X), NewClosure).

/* Reflexive, transitive closure membership check */
rt_closure_member((X,Y), Closure) :- member((X,Y), Closure), !.
rt_closure_member((X,X), _, Set) :- member(X, Set), !.
rt_closure_member((X,Y), Closure) :- member((X,Z), Closure), diff(Closure, [(X,_)], NewClosure), rt_closure_member((Z,Y), NewClosure).

/* Power set equality check */
power_set_equal(L1, L2) :- power_set(L1, P1), power_set(L2, P2), sets_equal(P1, P2), sets_equal(P2, P1), !.

sets_equal(P1, P2) :- match_sets(P1, P2), match_sets(P2, P1), !.

match_sets([], _).
match_sets([X|R], P) :- set_equal(X, P), match_sets(R, P).

set_equal([X], []) :- fail.
set_equal([X], [Y|R]) :- equal_sets(X, Y), !.
set_equal([X], [Y|R]) :- set_equal([X], R).

/* Cartesian product */
cartesian_product([], _, []).
cartesian_product(_, [], []).
cartesian_product([X|R], S, P) :- cartesian_product(R, S, P1), product(X, S, P2), append_lists(P2, P1, P).

product(_, [], []).
product(X, [Y|R], [(X,Y)|Z]) :- product(X, R, Z).

/* Power set calculation */
power_set([], [[]]).
power_set([X|R], P) :- power_set(R, P1), map_cons(X, P1, P2), append_lists(P2, P1, P).

/* Set intersection */
intersection([], _, []).
intersection([X|L], L1, [X|L2]) :- member(X, L1), intersection(L, L1, L2), !.
intersection([X|L], L1, L2) :- \+ member(X, L1), intersection(L, L1, L2), !.

/* Set difference */
diff([], _, []).
diff(S1, [], S1).
diff([X|S1], S2, S3) :- member(X, S2), diff(S1, S2, S3), !.
diff([X|S1], S2, [X|S3]) :- \+ member(X, S2), diff(S1, S2, S3), !.

/* Check for duplicate elements */
no_duplicates([]).
no_duplicates([X|R]) :- \+ member(X, R), no_duplicates(R).
