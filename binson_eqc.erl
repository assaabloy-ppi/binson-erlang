-module(binson_eqc).
-include_lib("eqc/include/eqc.hrl").
-export([writer_test/1, parser_test/1, parser_retest/0]).

-define(MAX_BINSON_LEN, 16#ffff).
-define(BOUND8,  (1 bsl 7)).
-define(BOUND16, (1 bsl 15)).
-define(BOUND32, (1 bsl 31)).

object_gen()     -> ?SIZED(Size, object_gen(Size)).
object_gen(0)    -> eqc_gen:map(string_gen(), primitive_gen());
object_gen(Size) -> eqc_gen:map(string_gen(), value_gen(Size div 2)).

value_gen(Size) -> frequency([{5, primitive_gen()},
                           {1, {array, array_gen(Size)}},
                           {1, object_gen(Size)}]).

primitive_gen() -> oneof([bool(), integer_gen(), real(), string_gen(), binary_gen()]).

integer_gen() -> oneof([choose(-?BOUND8, ?BOUND8),
                        choose(-?BOUND16, ?BOUND16),
                        choose(-?BOUND32, ?BOUND32),
                        largeint()]).

size_gen() -> oneof([nat(), choose(1, ?BOUND8+10)]).
string_gen()  -> ?LET(S, size_gen(), noshrink(vector(S+1, choose($A, $Z)))).
binary_gen()  -> ?LET(S, size_gen(), noshrink(binary(S+1))).

array_gen(0)    -> [primitive_gen()];
array_gen(Size) -> list(value_gen(Size div 2)).

% ------ binson c-light based writer (using NIFs) -------
write(true)  -> binson_nif:write_boolean(1);
write(false) -> binson_nif:write_boolean(0);

write(Int)    when is_integer(Int)  -> binson_nif:write_integer(Int);
write(Float)  when is_float(Float)  -> binson_nif:write_double(Float);
write(String) when is_list(String)  -> binson_nif:write_string(String);
write(Bytes)  when is_binary(Bytes) -> binson_nif:write_bytes(Bytes);

%%--- Composites encoding ------
write({array, Array_list}) ->
    binson_nif:write_array_begin(),
    [ write(El) || El <- Array_list ],
    binson_nif:write_array_end();

write(Object) when is_map(Object)->
    binson_nif:write_object_begin(),
    [ begin write(Name), write(Value) end || {Name, Value} <- maps:to_list(Object) ],
    binson_nif:write_object_end().

writer_check(Value) ->
    Ref_binson = binson:encode(Value),
    %io:format("Value: ~p~nBinson: ~p~n",[Value, Ref_binson]),
    if
        byte_size(Ref_binson) > ?MAX_BINSON_LEN -> true;
        true ->
            begin
                binson_nif:writer_reset(),
                write(Value),
                Ref_binson =:= binson_nif:writer_get_buf()
            end
    end.

% ------ binson c-light based parser (using NIFs) -------
parser_compare(true)  -> 1 =:= binson_nif:parser_get_boolean();
parser_compare(false) -> 0 =:= binson_nif:parser_get_boolean();
parser_compare(Int)    when is_integer(Int)  -> Int =:= binson_nif:parser_get_integer();
parser_compare(Float)  when is_float(Float)  -> Float =:= binson_nif:parser_get_double();
parser_compare(String) when is_list(String)  -> String =:= binson_nif:parser_get_string_copy();
parser_compare(Bytes)  when is_binary(Bytes) -> Bytes =:= binson_nif:parser_get_bytes_copy();

parser_compare({array, Array_list}) ->
    ok = binson_nif:parser_go_into_array(),
    Res = lists:all(fun(Elem) ->
                binson_nif:parser_next_array_value(),
                parser_compare(Elem)
              end, Array_list),
    ok = binson_nif:parser_go_up(),
    Res;

parser_compare(Object) when is_map(Object)->
    ok = binson_nif:parser_go_into(),
    Fields = lists:sort(maps:keys(Object)),
    Res = lists:all(fun(Field) ->
                ok = binson_nif:parser_field(Field),
                parser_compare(maps:get(Field, Object))
              end, Fields),
    ok = binson_nif:parser_go_up(),
    Res.

parser_check(Value) ->
    Binson = binson:encode(Value),
    %io:format("Value: ~p~nBinson: ~p~n",[Value, Binson]),
    if
        byte_size(Binson) > ?MAX_BINSON_LEN -> true;
        true ->
            begin
               binson_nif:parser_init(Binson),
               parser_compare(Value)
            end
    end.

%% ------ properties -------
prop_writer() -> ?FORALL(V, object_gen(), writer_check(V)).
prop_parser() -> ?FORALL(V, object_gen(), parser_check(V)).

%%  ------ testing functions -------
writer_test(N) ->
    binson_nif:writer_init(),
    eqc:quickcheck(eqc:numtests(N, prop_writer())).

parser_test(N) ->
    eqc:quickcheck(eqc:numtests(N, prop_parser())).

parser_retest() -> eqc:recheck(prop_parser()).
