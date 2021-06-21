all_a([]).
% all_a(H | T) :- H = a, all_a(T).
all_a([a|T]) :- all_a(T).

trans_a_b([], []).
trans_a_b([a|Ta], [b|Tb]) :- trans_a_b(Ta, Tb).

% scalarMult(Nr, L, R).
% scalarMult(Nr, L, [], R).

scalarMult(_, [], []).
scalarMult(Nr, [H|T], Lr) :-
    H2 is Nr * H,
    scalarMult(Nr, T, Tr),
    Lr = [H2|Tr].

scalarMult2(_, [], Lr, Lr).
scalarMult2(Nr, [H|T], Lp, Lr) :-
    H2 is Nr * H,
    append(Lp, [H2], Lp2),
    scalarMult2(Nr, T, Lp2, Lr).
scalarMult2(Nr, L, R) :- scalarMult2(Nr, L, [], R).

dot([], [], 0).
dot([H1|T1], [H2|T2], Rez) :-
    Hr is H1 * H2,
    dot(T1, T2, Rezp),
    Rez is Hr + Rezp.

dot2([], [], Lr, Lr).
dot2([H1|T1], [H2|T2], Rezp, Rez) :-
    Hr is H1 * H2,
    Rezp2 is Hr + Rezp,
    dot2(T1, T2, Rezp2, Rez).
dot2(L1, L2, Rez) :- dot2(L1, L2, 0, Rez).


















