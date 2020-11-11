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
    Shift = mmshift(M, E),
    Mv = 4 * Mf,
    io:fwrite("~1p~n", [[M, Mf, Mv]]),
    {Q, Vm, Vr, Vp, E10} = step_3(Ef, Mf, Shift),
    Accept = M rem 2 == 0,
    {VmIsTrailingZero, VrIsTrailingZero, Vp1} = bounds(Mv, Q, Vp, Accept, Ef - 2, Shift),
    {D1, E1} = compute_shortest(Vm, Vr, Vp1, VmIsTrailingZero, VrIsTrailingZero, Accept),
    % io:fwrite("~1p~n", [[E1, E10, integer_to_list(D1)]]),
    Ds = insert_decimal(E1 + E10, integer_to_list(D1)),
    insert_minus(S, Ds). 

-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).
-define(DOUBLE_BIAS, 1023).
-define(DOUBLE_MANTISSA_BITS, 52).
-define(DECODE_CORRECTION, 1075).

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
    {Mantissa, 1 - ?DECODE_CORRECTION};
decode(Mantissa, Exponent) ->
    {Mantissa + ?BIG_POW, Exponent - ?DECODE_CORRECTION}.

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).

log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).

mmshift(0, E) when E > 1 ->
    0;
mmshift(_M, _E) ->
    1.

halfway_compute(Mf, E, 0) when E > 1 ->
    X = 4 * Mf,
    {X - 1, X, X + 2};

halfway_compute(Mf, _E, _M) ->
    X = 4 * Mf,
    % io:fwrite("~1p~n", [[X]]),
    {X - 2, X, X + 2}.

-define(DOUBLE_POW5_INV_BITCOUNT, 125).
-define(DOUBLE_POW5_BITCOUNT, 125).

step_3(E2, Mv, Shift) when E2 >= 0 ->
    Q = max(0, ((E2 * 78913) bsr 18) - 1),
    E10 = Q,
    K = ?DOUBLE_POW5_INV_BITCOUNT + pow5bits(Q) - 1,
    I = -E2 + Q + K,
    % To_table = floor(math:pow(2, K) / math:pow(5, Q)) + 1,
    From_file = ryu_full_inv_table:table(Q),
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, I, From_file),
    io:fwrite("~1p~n", [[Vm, Vr, Vp]]),
    {Q, Vm, Vr, Vp, E10};

step_3(E2, Mv, Shift) when E2 < 0 ->
    % io:fwrite("~1p~n", [[E2, ((-E2 * 732923) bsr 20)]]),
    Q = max(0, ((-E2 * 732923) bsr 20) - 1),
    I = -E2 - Q,
    K = pow5bits(I) - ?DOUBLE_POW5_BITCOUNT,
    % To_table = floor(math:pow(5, I) / math:pow(2,K)),
    From_file = ryu_full_table:table(I),
    J = Q -K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, J, From_file),
    io:fwrite("~1p~n", [[Vm, Vr, Vp]]),
    E10 = E2 + Q,
    {Q, Vm, Vr, Vp, E10}.

mulShiftAll(Mv, Shift, I, Mul) ->
    A = ((Mv - 1 - Shift) * Mul) bsr I,
    B = (Mv * Mul) bsr I,
    C = ((Mv + 2) * Mul) bsr I,
    {A, B, C }.

pow5bits(E) ->
    ((E * 1217359) bsr 19) + 1.

pow5factor(Val) ->
    pow5factor(Val div 5, 0).

pow5factor(Val, Count) when (Val rem 5) /= 0->
    Count;
pow5factor(Val, Count) ->
    pow5factor(Val div 5, Count + 1).

multipleOfPowerOf5(Value, Q) ->
    pow5factor(Value) >= Q.


bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 21, Mv rem 5 =:= 0 ->
    {false, multipleOfPowerOf5(Mv, Q) , Vp};
bounds(Mv, Q, Vp, true, E2, Shift) when E2 >= 0, Q =< 21 ->
    {multipleOfPowerOf5(Mv - 1 - Shift, Q), false , Vp};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 21 ->
    {false, false , Vp - vpmodifier(multipleOfPowerOf5(Mv + 2, Q))};
bounds(_Mv, Q, Vp, true, E2, Shift) when E2 < 0, Q =< 1 ->
    {Shift =:= 1, true, Vp};
bounds(_Mv, Q, Vp, false, E2, _Shift) when E2 < 0, Q =< 1 ->
    {false, true, Vp - 1};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 < 0, Q < 63 ->
    {false, (Mv band ((1 bsl Q) -1 )) =:= 0, Vp};
bounds(_Mv, _Q, Vp, _Accept, _E2, _Shift) ->
    {false, false, Vp}.

vpmodifier(true) -> 1;
vpmodifier(false) -> 0.

compute_shortest(Vm, Vr, Vp, false, false, _Accept) ->
    % io:fwrite("~1p~n", [[Vm, Vr, Vp]]),
    {Vm1, Vr1, Removed, RoundUp} =
        general_case(Vm, Vr, Vp, 0, false),
    Output = Vr1 + handle_normal_output_mod(Vr1, Vm1, RoundUp),
    {Output, Removed};
compute_shortest(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, Accept) ->
    {Vm1, Vr1, VrTZ, Removed, LastRemovedDigit} = 
        handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, 0, 0),
    Output = Vr1 + handle_zero_output_mod(Vr1, Vm1, Accept, VrTZ, LastRemovedDigit),
    {Output, Removed}.

handle_trailing_zeros(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit)
    when (Vp div 10) =< (Vm div 10)->
    vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit);
handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, VrTZ, Removed, 0) ->
    VmTZ = VmIsTrailingZero and ((Vm rem 10) =:= 0),
    handle_trailing_zeros(Vm div 10, Vr div 10, Vp div 10, VmTZ, VrTZ, 1 + Removed, Vr rem 10);
handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, _VrTZ, Removed, _LastRemovedDigit) ->
    VmTZ = VmIsTrailingZero and ((Vm rem 10) =:= 0),
    handle_trailing_zeros(Vm div 10, Vr div 10, Vp div 10, VmTZ, false, 1 + Removed, Vr rem 10).

vmIsTrailingZero(Vm, Vr, Vp, false = VmTZ, VrTZ, Removed, LastRemovedDigit) ->
    handle_50_dotdot_0(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit);
vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit) when (Vm rem 10) /= 0 ->
    handle_50_dotdot_0(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit);
vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, 0) ->
    vmIsTrailingZero(Vm div 10, Vr div 10, Vp div 10, VmTZ, VrTZ, 1 + Removed, Vr rem 10);
vmIsTrailingZero(Vm, Vr, Vp, VmTZ, _VrTZ, Removed, _LastRemovedDigit) ->
    vmIsTrailingZero(Vm div 10, Vr div 10, Vp div 10, VmTZ, false, 1 + Removed, Vr rem 10).

handle_50_dotdot_0(Vm, Vr, _Vp, _VmTZ, true = VrTZ, Removed, 5) when (Vr rem 2) =:= 0 ->
    {Vm, Vr, VrTZ, Removed, 4};
handle_50_dotdot_0(Vm, Vr, _Vp, _VmTZ, VrTZ, Removed, LastRemovedDigit) ->
    {Vm, Vr, VrTZ, Removed, LastRemovedDigit}.

handle_zero_output_mod(_Vr, _Vm, _Accept, _VmTZ, LastRemovedDigit) 
    when LastRemovedDigit >= 5 ->
    1;
handle_zero_output_mod(Vr, Vm, Accept, VmTZ, _LastRemovedDigit) 
    when Vr =:= Vm; ((not Accept) or (not VmTZ)) ->
    1;
handle_zero_output_mod(_Vr, _Vm, _Accept, _VmTZ, _LastRemovedDigit) ->
    0.

general_case(Vm, Vr, Vp, Removed, _RU) when (Vp div 100) > (Vm div 100) ->
    VmD100 = Vm div 100,
    VrD100 = Vr div 100,
    VpD100 = Vp div 100,
    RoundUp = ((Vr rem 100) >= 50),
    general_case_10(VmD100, VrD100, VpD100,2 + Removed, RoundUp);
general_case(Vm, Vr, Vp, Removed, RoundUp) ->
    general_case_10(Vm, Vr, Vp, Removed, RoundUp).


general_case_10(Vm, Vr, Vp, Removed, RoundUp) 
    when (Vp div 10) =< (Vm div 10)->
    {Vm, Vr, Removed, RoundUp};
general_case_10(Vm, Vr, Vp, Removed, _RU) ->
    VmD10 = Vm div 10,
    VrD10 = Vr div 10,
    VpD10 = Vp div 10,
    RoundUp = ((Vr rem 10) >= 5),
    general_case_10(VmD10, VrD10, VpD10,1 + Removed, RoundUp).

handle_normal_output_mod(Vr, Vm, RoundUp) when (Vm =:= Vr) or RoundUp ->
    % io:fwrite("~1p~n", [[Vr, Vm, RoundUp]]),
    1;
handle_normal_output_mod(_Vr, _Vm, _RoundUp) ->
    % io:fwrite("~1p~n", [[Vr, Vm, RoundUp]]),
    0.

insert_decimal(-1, S) ->
    ["0.", S];
insert_decimal(0,S) ->
    [S, ".0"];
insert_decimal(1,S) ->
    [S, "0.0"];
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
                            [S0, ".", S1];
                        -Place - L =< ExpCost ->
                            ["0.", lists:duplicate(-Place - L, $0), S];
                        true ->
                            insert_exp(ExpL, S)
                    end;
                true ->
                    if
                        Place - L + 2 =< ExpCost ->
                            [S, lists:duplicate(Place - L + 1, $0), ".0"];
                        true ->
                            insert_exp(ExpL, S)
                    end
            end;
        true ->
            {S0, S1} = lists:split(Place, S),
            [S0, ".", S1]
    end.

insert_exp(ExpL, [C]) ->
    [C, ".0e", ExpL];
insert_exp(ExpL, [C | S]) ->
    [C, ".", S, "e", ExpL].

insert_minus(0, Digits) ->
    Digits;
insert_minus(1, Digits) ->
    [$- , Digits].