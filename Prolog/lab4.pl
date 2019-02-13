% Michael Smith
% Lab 4
% Started: 2/4/18
% Last Updated: 2/9/18


% Median Function take all numbers add them together and subtract the min and max
% to return the median.

% Other ways to do it...
% median(A,B,C, X) :- X is (A + B + C - (max(A,max(B,C)) + min(A,min(B,C)))).

% msort way went over in lab
% median(A,B,C, X) :- 
%     msort([A,B,C], [_,X,_]).

median(A,B,C, X) :-
    A >= B, A =< C, X is A;
    B >= A, B =< C, X is B;
    C >= A, C =< B, X is C;
    A >= C, A =< B, X is A;
    B >= C, B =< A, X is B;
    C >= B, C =< A, X is C.



% Contains Function takes in a list and returns all possible values.
contains([X|_], X).
contains([_|T], X) :-
    contains(T, X).

% For all y in L such that X >= y. 
largerEqual([], _).
largerEqual([H|T], X) :-
    X >= H,
    largerEqual(T, X).

% Only allowed one rule and can only use conains and largerEqual.
max(L,X) :-
    contains(L,X), largerEqual(L,X).
