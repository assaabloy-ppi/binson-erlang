-module(binson_nif).
-on_load(init/0).

-export([init/0, ver/0, writer_init/0, writer_reset/0, 
    write_boolean/1, write_integer/1, write_double/1, write_string/1, write_bytes/1,
    write_array_begin/0, write_array_end/0, write_object_begin/0, write_object_end/0,
    writer_get_counter/0, writer_get_buf/0,
    parser_init/1, parser_go_into_object/0, parser_go_upto_object/0,
    parser_go_into_array/0, parser_go_upto_array/0,
    parser_field/1, parser_next_array_value/0, 
    parser_get_boolean/0, parser_get_integer/0, parser_get_double/0,
    parser_get_string_copy/0, parser_get_bytes_copy/0]).

init() ->
      erlang:load_nif("./binson_nif", 0).
    
writer_init()               -> nif_error().
writer_reset()              -> nif_error().
write_boolean(_)            -> nif_error().
write_integer(_)            -> nif_error().
write_double(_)             -> nif_error().
write_string(_)             -> nif_error().
write_bytes(_)              -> nif_error().
write_array_begin()         -> nif_error().
write_array_end()           -> nif_error().
write_object_begin()        -> nif_error().
write_object_end()          -> nif_error().
writer_get_counter()        -> nif_error().
writer_get_buf()            -> nif_error().
parser_init(_)              -> nif_error().
parser_go_into_object()     -> nif_error().
parser_go_upto_object()     -> nif_error().
parser_go_into_array()      -> nif_error().
parser_go_upto_array()      -> nif_error().
parser_field(_)             -> nif_error().
parser_next_array_value()   -> nif_error().
parser_get_boolean()        -> nif_error().
parser_get_integer()        -> nif_error().
parser_get_double()         -> nif_error().
parser_get_string_copy()    -> nif_error().
parser_get_bytes_copy()     -> nif_error().

ver() -> nif_error().

nif_error() -> {error, nif_not_loaded}.
