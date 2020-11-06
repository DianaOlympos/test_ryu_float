defmodule TestRyuFloatTest do
  use ExUnit.Case
  use PropCheck

  property "same round-trip as :io_lib_format.fwrite_g/1", numtests: 1_000 do
    forall number <- float() do
      equals(
        :erlang.list_to_float(:ryu_float.fwrite_ryu(number)),
        :erlang.list_to_float(:io_lib_format.fwrite_g(number))
      )
    end
  end

  @tag timeout: 1_000 * 60 * 5
  property "same as :io_lib_format.fwrite_g/1", numtests: 1_000 do
    forall number <- float() do
      equals(:ryu_float.fwrite_ryu(number), :io_lib_format.fwrite_g(number))
    end
  end
end
