search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).


% === PART 1 ===


search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).

search_two(X, cons(X, cons(_, cons(X, _)))).
search_two(X, cons(_, T)) :- search_two(X, T).

search_anytwo(X, cons(X, T)) :- search(X, T).
search_anytwo(X, cons(H, T)) :- search_anytwo(X, T).


% === PART 2 ===


sum(X, zero, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).

size(nil, zero).
size(cons(_, T), s(S)) :- size(T, S).

sum_list(nil, zero).
sum_list(cons(H, T), S) :- sum(H, Sx, S), sum_list(T, Sx).
% sum_list(cons(zero, cons(s(s(zero)), cons(s(zero)  nil))), X).

greater(s(_), zero).
greater(s(N), s(M)) :- greater(N, M).

max(cons(H, nil), H).
max(cons(H, T), H) :- max(T, Tm), greater(H, Tm).
max(cons(H, T), Tm) :- max(T, Tm), greater(Tm, H).

minmax(cons(H, nil), H, H).
minmax(cons(H, T), Min, Max) :- minmax(T, Min, Max), greater(H, Min), greater(Max, H).
minmax(cons(H, T), H, Max) :- minmax(T, Min, Max), greater(Min, H), greater(Max, H).
minmax(cons(H, T), Min, H) :- minmax(T, Min, Max), greater(H, Min), greater(H, Max).
%minmax(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), Min, Max)


% === PART 3 ===


same(nil, nil).
same(cons(H, T1), cons(H, T2)) :- same(T1, T2).

all_bigger(cons(H1, nil), cons(H2, nil)) :- greater(H1, H2).
all_bigger(cons(H1, T1), cons(H2, T2)) :- greater(H1, H2), all_bigger(T1, T2).

% List1 should contain elements all also in List2
sublist(nil, List).
sublist(cons(H1, T1), List) :- search(H1, List), sublist(T1, List).


% === PART 4 ===


seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

seqR(zero, cons(zero, nil)).
seqR(s(N), cons(s(N), T)) :- seqR(N, T).

last(nil, E, cons(E, nil)).
last(cons(H, T), E, cons(H, M)) :- last(T, E, M).
%last(cons(a, cons(b, cons(c, nil))), d, X)

seqR2(s(zero), cons(zero, nil)).
seqR2(s(N), List) :- seqR2(N, P), last(P, N, List).

% == PART 5 ==

%Zips the values of two lists together. The lists must be with the same length.
zip(nil, nil, nil).
zip(cons(H1, T1), cons(H2, T2), cons((H1, H2), T3)) :- zip(T1, T2, T3).
%Example: zip(cons(a1, cons(a2, nil)), cons(b1, cons(b2, nil)), cons((a1, b1), cons((a2, b2), nil))).

%Deletes the first N values of a list.
drop(List, zero, List).
drop(cons(H, T), s(N), R) :- drop(T, N, R).
%Example: drop(cons(a, cons(b, cons(c, nil))), s(s(zero)), cons(c, nil)).

%Takes the first N elements of a list.
take(List, zero, nil).
take(cons(H, T), s(N), cons(H, R)) :- take(T, N, R).
%Example: take(cons(a, cons(b, cons(c, nil))), s(s(zero)), cons(a, cons(b, nil))).
