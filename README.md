# binson-erlang

This repo is an implementation of Binson (see [binson.org](http://binson.org/)) in Erlang.

 - Binson module for Erlang.
 - Binson module for Elixir.
 - Erlang QuickCheck-mini model for binson-c-light testing using Erlang Binson implementation as reference.


## Binson module for Erlang  
 Encoding function:


    binson:encode(Value).

 returns binary data.

 Decoding function:


    binson:decode(Binary).

returns decoded value and unparsed rest of binary input (if any).   

**Example:**

    sylbar@xubuntu-AA:~/binson-erlang$ erl
    Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

    Eshell V8.3  (abort with ^G)
    1> c(binson).
    {ok,binson}
    2> B = binson:encode("abc").
    <<20,3,97,98,99>>
    3> binson:decode(B).
    {"abc",<<>>}
    4>


**Matching Binson types to Erlang:**

Bool -> true/false atoms, example: true.  
Integer -> integer type, example: 1.  
Float -> float type, example: 1.0.  
String -> Erlang strings (char. lists), example: "abc".  
Bytes -> Erlang binaries, example: <<1,2,3>>.  
Array -> Erlang tuples, example: {1, "abc", {"sub", 1.0}}.  
Object -> Erlang maps, example: #{"field1" => "A", "field2" => {1,2,3}}.   

## Binson module for Elixir
 Encoding function:


    Binson.encode(value)

 returns binary data.

 Decoding function:


    Binson.decode(binary)

returns decoded value and unparsed rest of binary input (if any).   

**Example:**

    sylbar@xubuntu-AA:~/binson-erlang$ iex
    Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

    Interactive Elixir (1.4.1) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)> c("binson.ex")    
    [Binson]
    iex(2)> b = Binson.encode("abc")
    <<20, 3, 97, 98, 99>>
    iex(3)> Binson.decode(b)
    {"abc", ""}
    iex(4)>

**Matching Binson types to Elixir:**

Bool -> true/false atoms, example: true.  
Integer -> integer type, example: 1.  
Float -> float type, example: 1.0.  
String -> Elixir strings (binaries), example: "abc".  
Bytes -> Elixir binaries in :bin tagged tuple, example: {:bin, <<1,2,3>>}.  
Array -> Elixir lists, example: [1, "abc", ["sub", 1.0]].  
Object -> Elixir maps, example: %{"field1" => "A", "field2" => [1,2,3]}.   

## How to run Erlang QuickCheck Mini tests?

- Install Erlang.
- Install Erlang QuickCheck Mini from http://www.quviq.com/downloads/
- Compile:

	    ~/binson-erlang$ cd eqc_mini/
	    ~/binson-erlang/eqc_mini$ ./build.sh    

- Execute tests in Erlang shell:  

        ~/binson-erlang/eqc_mini$ cd build
        ~/binson-erlang/eqc_mini/build$ erl
        Erlang/OTP 22 [erts-10.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]
        Eshell V10.4  (abort with ^G)
	    1> binson_eqc:writer_test(100).
	    ...
	    2> binson_eqc:parser_test(100).
	    ...
