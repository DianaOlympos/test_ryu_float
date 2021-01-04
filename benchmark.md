# benchmark

## no JIT OTP 24

```text
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-8550U CPU @ 1.80GHz
Number of Available Cores: 8
Available memory: 23.25 GB
Elixir 1.11.2
Erlang 24.0-rc0

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 1.67 min
memory time: 2 s
parallel: 1
inputs: none specified
Estimated total run time: 3.47 min

Benchmarking OTP...
Benchmarking Ryu...

Name           ips        average  deviation         median         99th %
Ryu          77.68       12.87 ms    ±13.58%       12.41 ms       19.22 ms
OTP          25.32       39.50 ms    ±20.12%       36.63 ms       78.81 ms

Comparison: 
Ryu          77.68
OTP          25.32 - 3.07x slower +26.63 ms

Memory usage statistics:

Name    Memory usage
Ryu          7.44 MB
OTP          8.25 MB - 1.11x memory usage +0.81 MB
```

## JIT OTP 24

```text
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-8550U CPU @ 1.80GHz
Number of Available Cores: 8
Available memory: 23.25 GB
Elixir 1.11.2
Erlang 24.0-rc0

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 1.67 min
memory time: 2 s
parallel: 1
inputs: none specified
Estimated total run time: 3.47 min

Benchmarking OTP...
Benchmarking Ryu...

Name           ips        average  deviation         median         99th %
Ryu          91.28       10.96 ms    ±17.49%       10.32 ms       19.73 ms
OTP          36.90       27.10 ms    ±20.07%       25.52 ms       56.04 ms

Comparison: 
Ryu          91.28
OTP          36.90 - 2.47x slower +16.14 ms

Memory usage statistics:

Name    Memory usage
Ryu          7.48 MB
OTP          8.29 MB - 1.11x memory usage +0.81 MB
```

## Jason with JIT RYU

```text
##### With input Canada #####
Name                   ips        average  deviation         median         99th %
Jason                 4.80      208.25 ms     ±7.86%      209.20 ms      239.39 ms
Jason strict          4.60      217.44 ms    ±12.81%      214.80 ms      365.10 ms

Comparison: 
Jason                 4.80
Jason strict          4.60 - 1.04x slower +9.20 ms

Memory usage statistics:

Name            Memory usage
Jason               82.63 MB
Jason strict        82.63 MB - 1.00x memory usage +0.00072 MB
```

## Jason with JIT old

```text
##### With input Canada #####
Name                   ips        average  deviation         median         99th %
Jason                 5.27      189.58 ms    ±15.96%      182.57 ms      341.03 ms
Jason strict          5.10      196.22 ms    ±24.54%      187.09 ms      465.48 ms

Comparison: 
Jason                 5.27
Jason strict          5.10 - 1.04x slower +6.64 ms

Memory usage statistics:

Name            Memory usage
Jason               89.92 MB
Jason strict        89.92 MB - 1.00x memory usage +0.00072 MB
``` 