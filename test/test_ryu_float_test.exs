defmodule TestRyuFloatTest do
  use ExUnit.Case
  use PropCheck

  property "same round-trip as :io_lib_format.fwrite_g/1", numtests: 1_000 do
    forall number <- float() do
      equals(
        :erlang.binary_to_float(:erlang.iolist_to_binary(:ryu_float.fwrite_ryu(number))),
        :erlang.binary_to_float(:erlang.iolist_to_binary(:io_lib_format.fwrite_g(number)))
      )
    end
  end

  @tag timeout: 1_000 * 60 * 5
  property "same as :io_lib_format.fwrite_g/1", numtests: 1_000 do
    forall number <- float() do
      equals(
        :erlang.iolist_to_binary(:ryu_float.fwrite_ryu(number)),
        :erlang.iolist_to_binary(:io_lib_format.fwrite_g(number))
      )
    end
  end
end
