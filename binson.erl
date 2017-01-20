-module(binson).
-export([encode/1, decode/1]).

-define(BEGIN,      16#40). %%64
-define(END,        16#41). %%65
-define(BEGIN_ARR,  16#42). %%66
-define(END_ARR,    16#43). %%67
-define(TRUE,       16#44). %%68
-define(FALSE,      16#45). %%69
-define(DOUBLE,     16#46). %%70
-define(INT8,       16#10). %%16
-define(INT16,      16#11). %%17
-define(INT32,      16#12). %%18
-define(INT64,      16#13). %%19
-define(STR_LEN8,   16#14). %%20
-define(STR_LEN16,  16#15). %%21
-define(STR_LEN32,  16#16). %%22
-define(BYTE_LEN8,  16#18). %%24
-define(BYTE_LEN16, 16#19). %%25
-define(BYTE_LEN32, 16#1a). %%26

-define(BOUND8,  (1 bsl 7)).
-define(BOUND16, (1 bsl 15)).
-define(BOUND32, (1 bsl 31)).

%%------ Primitives encoding ------
encode(true)  -> <<?TRUE/integer>>;
encode(false) -> <<?FALSE/integer>>;

encode(Int) when is_integer(Int) ->
    if 
        Int >= -?BOUND8  andalso Int < ?BOUND8  -> <<?INT8/integer,  Int:8/integer-little>>;
        Int >= -?BOUND16 andalso Int < ?BOUND16 -> <<?INT16/integer, Int:16/integer-little>>;
        Int >= -?BOUND32 andalso Int < ?BOUND32 -> <<?INT32/integer, Int:32/integer-little>>;
        true                                    -> <<?INT64/integer, Int:64/integer-little>>
    end;

encode(Float) when is_float(Float) -> <<?DOUBLE/integer, Float/float-little>>;

encode(String) when is_list(String) -> 
    Len = length(String),
    Binary = list_to_binary(String), 
    if
        Len < 16#100   -> <<?STR_LEN8/integer,  Len:8/integer-little,  Binary/binary>>;
        Len < 16#10000 -> <<?STR_LEN16/integer, Len:16/integer-little, Binary/binary>>;
        true           -> <<?STR_LEN32/integer, Len:32/integer-little, Binary/binary>>
    end;

encode(Bytes) when is_binary(Bytes) -> 
    Len = byte_size(Bytes),
    if
        Len < 16#100   -> <<?BYTE_LEN8/integer,  Len:8/integer-little,  Bytes/binary>>;
        Len < 16#10000 -> <<?BYTE_LEN16/integer, Len:16/integer-little, Bytes/binary>>;
        true           -> <<?BYTE_LEN32/integer, Len:32/integer-little, Bytes/binary>>
    end;
     
%%------ Composites encoding ------
encode(Array) when is_tuple(Array)-> 
    Array_list = tuple_to_list(Array),
    Array_data = encode_array(Array_list, <<>>),
    <<?BEGIN_ARR/integer, Array_data/binary, ?END_ARR/integer>>;

encode(Object) when is_map(Object)-> 
    Fields = lists:sort(maps:keys(Object)),
    Obj_data = encode_object(Fields, Object, <<>>),
    <<?BEGIN/integer, Obj_data/binary, ?END/integer>>.

%%------ Composites encoding additional functions ------
encode_object([], _Object, Acc) -> Acc;
encode_object([Key|Tail], Object, Acc) ->
    Value = maps:get(Key, Object),
    Key_data = encode(Key),
    Value_data = encode(Value),
    encode_object(Tail, Object, <<Acc/binary, Key_data/binary, Value_data/binary>>).

encode_array([], Acc) -> Acc;
encode_array([Value|Tail], Acc) ->
    Value_data = encode(Value),
    encode_array(Tail, <<Acc/binary, Value_data/binary>>).

%%------ Primitives decoding ------

decode(<<?TRUE,  Rest/binary>>) -> {true, Rest};
decode(<<?FALSE, Rest/binary>>) -> {false, Rest};

decode(<<?INT8,  Int:8/integer-little,  Rest/binary>>) -> {Int, Rest};
decode(<<?INT16, Int:16/integer-little, Rest/binary>>) -> {Int, Rest};
decode(<<?INT32, Int:32/integer-little, Rest/binary>>) -> {Int, Rest};
decode(<<?INT64, Int:64/integer-little, Rest/binary>>) -> {Int, Rest};

decode(<<?DOUBLE, Float/float-little, Rest/binary>>) -> {Float, Rest};

decode(<<?STR_LEN8,  Len:8/integer-little,  Str:Len/binary, Rest/binary>>) -> {binary_to_list(Str), Rest};
decode(<<?STR_LEN16, Len:16/integer-little, Str:Len/binary, Rest/binary>>) -> {binary_to_list(Str), Rest};
decode(<<?STR_LEN32, Len:32/integer-little, Str:Len/binary, Rest/binary>>) -> {binary_to_list(Str), Rest};

decode(<<?BYTE_LEN8,  Len:8/integer-little,  Bytes:Len/binary, Rest/binary>>) -> {Bytes, Rest};
decode(<<?BYTE_LEN16, Len:16/integer-little, Bytes:Len/binary, Rest/binary>>) -> {Bytes, Rest};
decode(<<?BYTE_LEN32, Len:32/integer-little, Bytes:Len/binary, Rest/binary>>) -> {Bytes, Rest};

%%------ Composites decoding ------
decode(<<?BEGIN_ARR, Rest/binary>>) -> decode_array(Rest, []);
decode(<<?BEGIN, Rest/binary>>) -> decode_object(Rest, #{}).

%%------ Composites decoding additional functions ------
decode_object(<<?END, Rest/binary>>, Acc) -> {Acc, Rest};
decode_object(Data, Acc) ->
    {Key, Val_Rest} = decode(Data),
    {Value, Rest}   = decode(Val_Rest),
    decode_object(Rest, maps:put(Key, Value, Acc)).

decode_array(<<?END_ARR, Rest/binary>>, Acc) -> {list_to_tuple(lists:reverse(Acc)), Rest};
decode_array(Data, Acc) ->
    {Value, Rest}   = decode(Data),
    decode_array(Rest, [Value|Acc]).
