list = StreamData.float()
|> Enum.take(10_000)

Benchee.run(
  %{
    "OTP" => fn -> Enum.each(list, &:ryu_float.fwrite_g/1) end,
    "Ryu" => fn -> Enum.each(list, &:ryu_float.fwrite_ryu/1) end
  },
  memory_time: 2,
  time: 100
)
