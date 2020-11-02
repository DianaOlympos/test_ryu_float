-module(ryu_float).

-export([fwrite_g/1, fwrite_ryu/1, log2floor/1]).

fwrite_g(Float) ->
    io_lib_format:fwrite_g(Float).

% TODO handle infinity, NaN and 0
fwrite_ryu(0.0) ->
    "0.0";

fwrite_ryu(Float) ->
    {S, M, E} = sign_mantissa_exponent(Float),
    {Mf, Ef} = decode(M, E),
    E2 = Ef - 2,
    % io:fwrite("~1p~n", [[M, E, Mf, Ef]]),
    {U, V, W} = halfway_compute(Mf, E, M),
    {_Q, A, B, C, E10, Za, Break_tie, Zc} = step_3(E2, U, V, W),
    % io:fwrite("~1p~n", [[Q]]),
    Accept = M rem 2 == 0,
    Accept_smaller = Accept and Za,
    Accept_larger = Accept or not Zc,
    io:fwrite("~1p~n", [[Accept, Za]]),
    {D1, E1} = compute_shortest(A, B, C, Accept_smaller, Accept_larger, Break_tie),
    % io:fwrite("~1p~n", [[E1, E10, integer_to_list(D1)]]),
    Ds = insert_decimal(E1 + E10, integer_to_list(D1)),
    insert_minus(S, Ds).

-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).
-define(DOUBLE_BIAS, 1023).
-define(DOUBLE_MANTISSA_BITS, 52).

% sign_mantissa_exponent(F) ->
%     case <<F:64/float>> of
%         <<S:1, 0:11, M:52>> -> % denormalized
%             {S, M, 0};
%         <<S:1, BE:11, M:52>> when BE < 2047 ->
%             {S, M , BE}
%     end.

sign_mantissa_exponent(F) ->
    case <<F:64/float>> of
        <<S:1, BE:11, M:52>> when BE < 2047 ->
            {S, M , BE}
    end.

decode(Mantissa, 0) ->
    % E = log2floor(Mantissa),
    {Mantissa, 1 - ?DOUBLE_BIAS - ?DOUBLE_MANTISSA_BITS};
decode(Mantissa, Exponent) ->
    {Mantissa + ?BIG_POW, Exponent - ?DOUBLE_BIAS - ?DOUBLE_MANTISSA_BITS}.

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).

log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).

halfway_compute(Mf, E, 0) when E > 1->
    X = 4 * Mf,
    {X - 1, X, X + 2};

halfway_compute(Mf, _E, _M) ->
    X = 4 * Mf,
    io:fwrite("~1p~n", [[X]]),
    {X - 2, X, X + 2}.

-define(DOUBLE_POW5_INV_BITCOUNT, 122).
-define(DOUBLE_POW5_BITCOUNT, 121).

step_3(E2, U, V, W) when E2 >= 0 ->
    Q = max(0, ((E2 * 78913) bsr 18) - 1),
    K = ?DOUBLE_POW5_INV_BITCOUNT + pow5bits(Q) - 1,
    I = -E2 + Q + K,
    To_table = floor(math:pow(2, K) / math:pow(5, Q)) + 1,
    A = (U*To_table) bsr I,
    B = (V*To_table) bsr I,
    C = (W*To_table) bsr I,
    E10 = Q,
    Pow5Qm = case Q of
                0 -> 1;
                _ -> floor(math:pow(5, Q - 1))
            end,
    Pow5Q = case Q of
                0 -> 1;
                _ -> Pow5Qm * 5
            end,
    Za = (U rem Pow5Q) == 0 ,
    Zb = (V rem Pow5Qm) == 0,
    Zc = (W rem Pow5Q) == 0,
    {Q, A, B, C, E10, Za, Zb, Zc};

step_3(E2, U, V, W) when E2 < 0 ->
    % io:fwrite("~1p~n", [[E2, ((-E2 * 732923) bsr 20)]]),
    Q = max(0, ((-E2 * 732923) bsr 20) - 1),
    I = -E2 - Q,
    K = pow5bits(I) - ?DOUBLE_POW5_BITCOUNT,
    To_table = floor(math:pow(5, I) / math:pow(2,K)),
    J = Q -K,
    A = (U * To_table) bsr J,
    B = (V * To_table) bsr J,
    C = (W * To_table) bsr J,
    E10 = E2 + Q,
    Za = (U bsr Q) == 0 ,
    Zb = (V bsr (Q - 1)) == 0,
    Zc = (W bsr Q) == 0,
    io:fwrite("~1p~n", [[A, Za]]),
    {Q, A, B, C, E10, Za, Zb, Zc}.

pow5bits(E) ->
    ((E * 1217359) bsr 19) + 1.

pow5factor(Val) when (Val rem 5) /= 0 ->
    1;
pow5factor(Val) ->
    pow5factor(Val div 5, 1).

pow5factor(Val, Count) when (Val rem 5) /= 0->
    Count;
pow5factor(Val, Count) when Val =< 0->
    pow5factor(Val div 5, Count + 1).

multipleOfPowerOf5(Value, Q) ->
    pow5factor(Value) >= Q.


compute_shortest(A, B, C, Accept_smaller, Accept_larger, Break_ties) ->
    Ci = C - cmodifier(Accept_larger),
    {A1, B1, C1, Digit, All_A, All_B, I} = 
      loop_1(A, B, Ci, 0, true, true, 0),
    io:fwrite("~1p~n", [[Accept_smaller, All_A]]),
    if 
        Accept_smaller andalso All_A ->
          loop_2(A1, B1, C1, Digit, All_A, All_B, I, Break_ties);
        true -> {C1, I}
    end.

cmodifier(true) -> 0;
cmodifier(_) -> 1.

loop_1(A, B, C, Digiti, All_A_zero, All_B_zero, I)
    when (A div 10) < (C div 10) ->
    A_zero = All_A_zero and ((A rem 10) == 0),
    Ai = A div 10,
    Ci = C div 10,
    Bi = B div 10,
    Digit = B rem 10,
    B_zero = All_B_zero and (Digiti == 0),
    loop_1(Ai, Bi, Ci, Digit, A_zero, B_zero, 1 + I);

loop_1(Ai, Bi, Ci, Digiti, All_A_zero, All_B_zero, I) ->
    {Ai, Bi, Ci, Digiti, All_A_zero, All_B_zero, I}.

loop_2(A, B, C, Digiti, Aa, All_B_zero, I, Bt) when (A rem 10 == 0) ->
    Ai = A div 10,
    Ci = C div 10, 
    Bi = B div 10,
    Digit = B rem 10,
    B_zero = All_B_zero and (Digiti == 0),
    loop_2(Ai, Bi, Ci, Aa, Digit, B_zero, 1 + I, Bt);

loop_2(A, B, C, Digit, All_A, All_B, I, Break_tie_down) ->
    return(A, B, C, Digit, All_A, All_B, I, Break_tie_down).

return(A, B, C, Digiti, All_A_zero, All_B_zero, I, Break_tie_down) ->
    Is_tie = (Digiti == 5) and All_B_zero,
    Want_round_down = (Digiti < 5) or (Is_tie and Break_tie_down),
    Round_down =
        Want_round_down and ((A /= B) or All_A_zero) or ((B + 1) > C),
    io:fwrite("~1p~n", [[B, Round_down, Want_round_down, Is_tie]]),
    {B + bmod(Round_down), I}.

bmod(true) -> 0;
bmod(_) -> 1.

insert_decimal(-1, S) ->
    "0." ++ S;
insert_decimal(0,S) ->
    S ++ ".0";
insert_decimal(1,S) ->
    S ++ "0.0";
insert_decimal(Place, S) ->
    L = length(S),
    % io:fwrite("~1p~n", [[Place, S, L]]),
    if
        Place < 0;
        Place >= L ->
            ExpL = integer_to_list(Place - 1),
            ExpDot = if L =:= 1 -> 2; true -> 1 end,
            ExpCost = length(ExpL) + 1 + ExpDot,
            if 
                Place < 0 ->
                    if 
                        L > abs(Place) ->
                            {S0, S1} = lists:split(L + Place, S),
                            S0 ++ "." ++ S1;
                        -Place - L =< ExpCost ->
                            "0." ++ lists:duplicate(-Place - L, $0) ++ S;
                        true ->
                            insert_exp(ExpL, S)
                    end;
                true ->
                    if
                        Place - L + 2 =< ExpCost ->
                            S ++ lists:duplicate(Place - L + 1, $0) ++ ".0";
                        true ->
                            insert_exp(ExpL, S)
                    end
            end;
        true ->
            {S0, S1} = lists:split(Place, S),
            S0 ++ "." ++ S1
    end.

insert_exp(ExpL, [C]) ->
    [C] ++ ".0e" ++ ExpL;
insert_exp(ExpL, [C | S]) ->
    [C] ++ "." ++ S ++ "e" ++ ExpL.

insert_minus(0, Digits) ->
    Digits;
insert_minus(1, Digits) ->
    [$- | Digits].