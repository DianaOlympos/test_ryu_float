-module(generate_table).
-export([write_table_module/0, write_inv_table_module/0]).

-define(TABLE_SIZE, 326).
-define(INV_TABLE_SIZE, 342).

-define(POW5_BITCOUNT, 125).
-define(POW5_INV_BITCOUNT, 125).

write_table_module() ->
  Module = "src/ryu_full_table.erl",
  List = [ values(X) || X <- lists:seq(0, ?TABLE_SIZE - 1)],
  file:write_file(Module,
    ["-module(ryu_full_table). \n-export([table/1]). \n",
      [["table(",
        io_lib:format("~p", [Key]),
        ") -> ",
      io_lib:format("~p", [Val]), ";\n"
      ] || {Key, Val} <- List],
    "table(_) -> error(function_clause). "]).

write_inv_table_module() ->
  Module = "src/ryu_full_inv_table.erl",
  List = [ inv_values(X) || X <- lists:seq(0, ?INV_TABLE_SIZE - 1)],
  file:write_file(Module,
    ["-module(ryu_full_inv_table). \n-export([table/1]). \n",
      [["table(",
        io_lib:format("~p", [Key]),
        ") -> ",
      io_lib:format("~p", [Val]), ";\n"
      ] || {Key, Val} <- List],
    "table(_) -> error(function_clause). \n"]).

-define(MASK, ((1 bsl 64) - 1)).

inv_values(X) ->
  Pow = pow5(X),
  Pow5len = log2floor(Pow),
  J = Pow5len + ?POW5_INV_BITCOUNT - 1,
  Inv = ((1 bsl J) div Pow) + 1,
  % Pow5high = Inv bsr 64,
  % Pow5low = Inv band ?MASK,
  {X, Inv}.

values(X) ->
  Pow = pow5(X),
  Pow5len = log2floor(Pow),
  Pow5 = Pow bsr (Pow5len - ?POW5_BITCOUNT),
  % Pow5high = Pow5 bsr 64,
  % Pow5low = Pow5 band ?MASK,
  {X, Pow5}.

pow5(0) ->
  1;
pow5(1) ->
  5;
pow5(X) ->
  5 * pow5(X - 1).

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).

log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).
