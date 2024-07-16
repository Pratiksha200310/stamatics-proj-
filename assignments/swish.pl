##Assigment 2
% Syntax of expressions (Exp)
exp(N).                 % Constants (e.g., numbers)
exp(T).                 % True constant
exp(F).                 % False constant
exp(x).                 % Variable
exp(plus(E1, E2)).      % Addition
exp(sub(E1, E2)).       % Subtraction
exp(mult(E1, E2)).      % Multiplication
exp(div(E1, E2)).       % Division
exp(mod(E1, E2)).       % Modulo
exp(exp(E1, E2)).       % Exponentiation
exp(and(E1, E2)).       % Logical AND
exp(or(E1, E2)).        % Logical OR
exp(not(E)).            % Logical NOT
exp(implies(E1, E2)).   % Logical implication
exp(xor(E1, E2)).       % Logical XOR
exp(eq(E1, E2)).        % Equality
exp(neq(E1, E2)).       % Not Equal
exp(gt(E1, E2)).        % Greater than
exp(gte(E1, E2)).       % Greater than or equal to
exp(lt(E1, E2)).        % Less than
exp(lte(E1, E2)).       % Less than or equal to
% Type checking relation
hastype(G, N, intT) :- exp(N), integer(N).
hastype(G, T, boolT) :- exp(T), (T = true ; T = false).
hastype(G, x, T) :- member((x, T), G).

% Arithmetic operations
hastype(G, plus(E1, E2), intT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).
hastype(G, sub(E1, E2), intT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).
hastype(G, mult(E1, E2), intT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).
hastype(G, div(E1, E2), intT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT),
    E2 \= 0.  % Ensure division by zero is avoided (optional)
hastype(G, mod(E1, E2), intT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT),
    E2 \= 0.  % Ensure modulo by zero is avoided (optional)
hastype(G, exp(E1, E2), intT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).

% Boolean operations
hastype(G, and(E1, E2), boolT) :-
    hastype(G, E1, boolT),
    hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :-
    hastype(G, E1, boolT),
    hastype(G, E2, boolT).
hastype(G, not(E), boolT) :-
    hastype(G, E, boolT).
hastype(G, implies(E1, E2), boolT) :-
    hastype(G, E1, boolT),
    hastype(G, E2, boolT).
hastype(G, xor(E1, E2), boolT) :-
    hastype(G, E1, boolT),
    hastype(G, E2, boolT).

% Comparison operations
hastype(G, eq(E1, E2), boolT) :-
    hastype(G, E1, T),
    hastype(G, E2, T).
hastype(G, neq(E1, E2), boolT) :-
    hastype(G, E1, T),
    hastype(G, E2, T).
hastype(G, gt(E1, E2), boolT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).
hastype(G, gte(E1, E2), boolT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).
hastype(G, lt(E1, E2), boolT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).
hastype(G, lte(E1, E2), boolT) :-
    hastype(G, E1, intT),
    hastype(G, E2, intT).

