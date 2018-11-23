istriad(0, []).
istriad(N, [F|Fs]) :- N>0, NewN is N div 3, F is N mod 3, istriad(NewN, Fs).

isbalanced(N, [0|Es], Left, Right, Order, 0) :- NewO is Order + 1, isbalanced(N, Es, Left, Right, NewO, 0).
isbalanced(N, [1|Es], Left, Right, Order, 0) :- NewO is Order + 1, Right = [H|NewR], H is Order, isbalanced(N, Es, Left, NewR, NewO, 0).
isbalanced(N, [2|Es], Left, Right, Order, 0) :- NewO is Order + 1, Left = [H|NewL], H is Order, isbalanced(N, Es, NewL, Right, NewO, 1).
isbalanced(N, [], Left, Right, Order, 1) :- isbalanced(N, [1], Left, Right, Order, 0).
isbalanced(N, [E|Es], Left, Right, Order, 1) :- E<2->(NewE is E+1, isbalanced(N, [NewE|Es], Left, Right, Order, 0)); (NewO is Order+1, isbalanced(N, Es, Left, Right, NewO, 1)).
isbalanced(N, [], [], [], Order, 0) :- Order-1 =< N.

balance(N, W, L, R) :- istriad(W, W3), isbalanced(N, W3, L, R, 1, 0).
