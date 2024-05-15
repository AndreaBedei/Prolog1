search(X, cons(X , _)).
search(X, cons(_, Xs)) :- search(X, Xs).

search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).


search_two(X, cons(X, cons(Y, cons(X, _)))).
search_two(X, cons(_, Xs)) :- search_two(X, Xs).

search_anytwo(X, cons(X, Xs)) :- search(X, Xs).
search_anytwo(X, cons(_, Xs)) :- search_anytwo(X, Xs).


size(nil, zero).
size(cons(X, Xs), s(Y)) :- size(Xs, Y).

sum(X, zero, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).

sum_list(nil, zero).
sum_list(cons(X, Xz), Y) :- sum_list(Xz, O), sum(X, O, Y).


count(List, E, N) :- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E2, N, M).

greater(s(_), zero).
greater(s(X), s(Y)) :- greater(X, Y).

max(List, Max) :- max(List, zero, Max).
max(nil, Max, Max).
max(cons(H, T), TempMax, Max) :- greater(H, TempMax), max(T, H, Max).
max(cons(_, T), TempMax, Max) :- max(T, TempMax, Max).

lower(zero, s(_)).
lower(s(X), s(Y)) :- greater(X, Y).

minMax(cons(H, T), Min, Max) :- minMax(T, H, H, Min, Max).
minMax(nil, Max, Min, Min, Max).
minMax(cons(H, T), TempMax, TempMin, Min, Max) :- greater(H, TempMax), minMax(T, H, TempMin, Min, Max).
minMax(cons(H, T), TempMax, TempMin, Min, Max) :- lower(H, TempMin), minMax(T, TempMax, H, Min, Max).
minMax(cons(H, T), TempMax, TempMin, Min, Max) :- minMax(T, TempMax, TempMin, Min, Max).


same(nil, nil).
same(cons(H, T1), cons(H, T2)) :- same(T1, T2).


all_bigger(nil, nil).
all_bigger(cons(H1, T1), cons(H2, T2)) :- greater(H1, H2), all_bigger(T1, T2).


sublist(nil, cons(H, T)).
sublist(cons(H, T), cons(H2, T2)) :- search(H, cons(H2, T2)), sublist(T, cons(H2, T2)).

seq(zero, _, nil).
seq(s(N), E, cons(E ,T)) :- seq(N, E, T).


seqR(zero, nil).
seqR(s(R), cons(R, T)) :- seqR(R, T).


last(nil, X, cons(X, nil)).
last(cons(H, nil), X, cons(H, cons(X, nil))).
last(cons(H, T), X, cons(H, OUTPUT)) :- last(T, X, OUTPUT).
 
seqR2(zero, nil).
seqR2(s(zero), cons(zero, nil)).
seqR2(s(X), RES) :- seqR2(X, OUTPUT), last(OUTPUT, X, RES).

lastElement(cons(H, nil), H).
lastElement(cons(_, T), OUTPUT) :- lastElement(T, OUTPUT).
 
map(cons(X, nil), s(X)).
map(cons(R, T), cons(s(R),V)) :- map(T, V).
 
 
filter(cons(X, nil), cons(X,nil)) :- greater(X, zero).
filter(cons(X, nil), nil).
filter(cons(H, T), cons(H, V)) :- filter(T, V), greater(H, zero).
filter(cons(H, T), V) :- filter(T, V).
 
countElements(nil, zero).
countElements(cons(H, T), s(OUTPUT)) :- countElements(T, OUTPUT), greater(H, zero).
countElements(cons(H, T), OUTPUT) :- countElements(T, OUTPUT).




