# binson-erlang
1. Binson implementation in Erlang.
2. QuickCheck mini model for binson-c-light testing using Binson implementation as reference.
 
How to run QuickCheck test?
1. Install Erlang.
2. Install Erlang QuickCheck mini from http://www.quviq.com/downloads/
3. Compile:
   ~/binson-erlang$ ./build.sh
4. Execute tests in Erlang shell:
~/binson-erlang/build$ erl
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> binson_eqc:writer_test(100).
...
2> binson_eqc:parser_test(100).
...
5. Exit Erlang with q().

