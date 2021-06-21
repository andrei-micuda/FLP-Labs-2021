distance((X1, Y1), (X2, Y2), R) :-
    R is sqrt((X1-X2)**2 + (Y1-Y2)**2).

fib(0, 1).
fib(1, 1).
%fib(N, R) :-
%    N1 is N-1,
%    N2 is N-2,
%    fib(N1, R1),
%    fib(N2, R2),
%    R is R1 + R2.
fib(N, R) :- fibEff(N, R, _).

fibEff(1, 1, 0).
fibEff(N, Fn, Fn1) :-
    N > 1,
    N1 is N-1,
    fibEff(N1, Fn1, Fn2),
   	Fn is Fn1 + Fn2.

writeLine(0, _) :- nl.
writeLine(N, Chr) :-
    write(Chr),
    N1 is N-1,
    writeLine(N1, Chr).
    

square(N, Chr) :- squareAux(N, N, Chr).

squareAux(0, _, _).
squareAux(Lin, Col, Chr) :-
    writeLine(Col, Chr),
    Lin1 is Lin-1,
    squareAux(Lin1, Col, Chr).

%-- LISTE
elements_of(X, [X | _]).
elements_of(X, [_ | T]) :- elements_of(X, T).

concat_lists([], L2, L2).
concat_lists([H|T], L2, [H | TRez]) :-
    concat_lists(T, L2, TRez).

all_chr([], _).
all_chr([Chr|T], Chr) :-
    all_chr(T, Chr).

all_a(Lst) :- all_chr(Lst, a).

%trans_a_b(LstA, LstB) :-
%    all_chr(LstA, a),
%    all_chr(LstB, b),
%    length(LstA, LenA),
%    length(LstB, LenA).

trans_a_b([a|Ta], [b|Tb]) :- trans_a_b(Ta, Tb).

% scalarMult(Scalar, Lst, Rez)
scalarMult(_, [], []).
scalarMult(S, [H|T], [HRez| TRez]) :- 
    HRez is S * H,
    scalarMult(S, T, TRez).

dot([], [], 0).
dot([H1|T1], [H2|T2], Rez) :-
    HRez is H1 * H2,
    dot(T1, T2, TRez),
    Rez is HRez + TRez.

max([H], H).
max([H|T], Rez) :-
    max(T, RezP),
    (
    	(H > RezP, Rez is H);
    	Rez is RezP
    ).

rev([], []).
rev([H|T], LRev) :-
    rev(T, TRev),
    append(TRev, [H], LRev).

palindrome(Lst) :-
    rev(Lst, LstRev),
    Lst = LstRev.

% not_in(Elem, Lst)
not_in(_, []).
not_in(Elem, [H|T]) :-
    H \= Elem,
    not_in(Elem, T).

remove_duplicates([], []).
remove_duplicates([H|T], Rez) :-
    remove_duplicates(T, TRez),
    (
    	(not_in(H, TRez), Rez = [H|TRez]);
    	Rez = TRez
    ).