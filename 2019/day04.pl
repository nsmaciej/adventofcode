digit(X) :-
    numlist(0, 9, Y),
    member(X, Y).

ordered([]).
ordered([_]).
ordered([X, Y|Rest]) :-
    X=<Y,
    ordered([Y|Rest]).

valid(Lower, Upper, Soln) :-
    length(Soln, 6),
    maplist(digit, Soln),
    Lower@<Soln,
    Soln@<Upper,
    nextto(Repeated, Repeated, Soln),
    ordered(Soln).
    
solution(Lower, Upper, Length) :-
    setof(X, valid(Lower, Upper, X), Bag),
    length(Bag, Length).

my_solution(X) :-
    solution([1, 2, 4, 0, 7, 5], [5, 8, 0, 7, 6, 9], X).
