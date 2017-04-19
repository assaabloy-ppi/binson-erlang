/* binson_nif.c */
#include "erl_nif.h"
#include "binson_light.h"

#define MAX_WRITER_BUF (65536)

uint8_t        writer_buf[MAX_WRITER_BUF];
binson_writer  w;

uint8_t        parser_buf[MAX_WRITER_BUF];
binson_parser  p;

 
static ERL_NIF_TERM ver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Binson Light NIF!", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM writer_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_writer_init(&w, writer_buf, MAX_WRITER_BUF-1);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM writer_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_writer_reset(&w);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_integer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 eval; 
    
    if (!enif_get_int64(env, argv[0], &eval)) return enif_make_badarg(env);  
    
    binson_write_integer(&w, eval);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_boolean(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int eval; 
    
    if (!enif_get_int(env, argv[0], &eval)) return enif_make_badarg(env);
   
    if (eval) binson_write_boolean(&w, true);
    else      binson_write_boolean(&w, false);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double eval; 
    
    if (!enif_get_double(env, argv[0], &eval)) return enif_make_badarg(env);  
    
    binson_write_double(&w, eval);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len; 
    char* string_val;
    
    if (!enif_get_list_length(env, argv[0], &len)) return enif_make_badarg(env);  
    len++; 
    
    if (!(string_val = malloc(len))) return enif_make_atom(env, "no_mem");
    
    enif_get_string(env, argv[0], string_val, len, ERL_NIF_LATIN1);
    binson_write_string(&w, string_val);
    free(string_val);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_bytes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    uint8_t* bin_data;

    if (!enif_inspect_binary(env, argv[0], &bin)) return enif_make_badarg(env);  
    if (!(bin_data = malloc(bin.size)))  return enif_make_atom(env, "no_mem");
    
    memcpy(bin_data, bin.data, bin.size);
    binson_write_bytes(&w, bin_data, bin.size);
    free(bin_data);
    return enif_make_atom(env, "ok");

}

static ERL_NIF_TERM write_array_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_write_array_begin(&w);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_array_end(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_write_array_end(&w);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_object_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_write_object_begin(&w);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_object_end(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_write_object_end(&w);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM writer_get_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int cnt = binson_writer_get_counter( &w );
    return enif_make_int(env, cnt);
}

static ERL_NIF_TERM writer_get_buf(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary binary;
    int size = binson_writer_get_counter( &w );   
    if (!enif_alloc_binary(size, &binary)) return enif_make_atom(env, "no_mem");  
    memcpy(binary.data, writer_buf, size);   
    return enif_make_binary(env, &binary);
}

static ERL_NIF_TERM parser_result(ErlNifEnv* env)
{
    switch (p.error_flags)
    {
        case BINSON_ID_OK:
            return enif_make_atom(env, "ok");
        case BINSON_ID_BUF_FULL:
            return enif_make_atom(env, "error:BINSON_ID_BUF_FULL");
        case BINSON_ID_PARSE_NO_FIELD_NAME:
            return enif_make_atom(env, "error:BINSON_ID_PARSE_NO_FIELD_NAME");
        case BINSON_ID_PARSE_END_OF_OBJECT:
            return enif_make_atom(env, "error:BINSON_ID_PARSE_END_OF_OBJECT");
        case BINSON_ID_PARSE_WRONG_STATE:
            return enif_make_atom(env, "error:BINSON_ID_PARSE_WRONG_STATE");
        case BINSON_ID_PARSE_WRONG_TYPE:
            return enif_make_atom(env, "error:BINSON_ID_PARSE_WRONG_TYPE");
        case BINSON_ID_PARSE_BAD_LEN:
            return enif_make_atom(env, "error:BINSON_ID_PARSE_BAD_LEN");
        case BINSON_ID_PARSE_END_OF_BUFFER:
            return enif_make_atom(env, "error:BINSON_ID_PARSE_END_OF_BUFFER");
        default:
            return enif_make_atom(env, "error:other");
    }
}

static ERL_NIF_TERM parser_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
 
    if (!enif_inspect_binary(env, argv[0], &bin)) return enif_make_badarg(env);
 
    memcpy(parser_buf, bin.data, bin.size);
    binson_parser_init(&p, parser_buf, bin.size);
    
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM parser_go_into_object(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_parser_go_into_object(&p);
    return parser_result(env);
}

static ERL_NIF_TERM parser_go_upto_object(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_parser_go_upto_object(&p);
    return parser_result(env);
}

static ERL_NIF_TERM parser_go_into_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_parser_go_into_array(&p);
    return parser_result(env);
}

static ERL_NIF_TERM parser_go_upto_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_parser_go_upto_array(&p);
    return parser_result(env);
}

static ERL_NIF_TERM parser_field(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len; 
    char* string_val;
    
    if (!enif_get_list_length(env, argv[0], &len)) return enif_make_badarg(env);  
    len++; 
    
    if (!(string_val = malloc(len))) return enif_make_atom(env, "no_mem");
    
    enif_get_string(env, argv[0], string_val, len, ERL_NIF_LATIN1); 
    binson_parser_field(&p, string_val);
    free(string_val);
    
    return parser_result(env);
}

static ERL_NIF_TERM parser_next_array_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    binson_parser_next_array_value(&p);
    return parser_result(env);
}

static ERL_NIF_TERM parser_get_boolean(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint8_t int_val = binson_parser_get_boolean(&p);
        
    if (BINSON_ID_OK == p.error_flags) return enif_make_int(env, int_val); 
    else return parser_result(env);
}

static ERL_NIF_TERM parser_get_integer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int64_t int_val = binson_parser_get_integer(&p);
    
    if (BINSON_ID_OK == p.error_flags) return enif_make_int64(env, int_val);
    else return parser_result(env);
}

static ERL_NIF_TERM parser_get_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double int_val = binson_parser_get_double(&p);
    
    if (BINSON_ID_OK == p.error_flags) return enif_make_double(env, int_val);
    else return parser_result(env);
}

static ERL_NIF_TERM parser_get_string_copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char* string_val;
    binson_tok_size len = binson_parser_get_string_len(&p)+1;
    
    if (!(string_val = malloc(len))) return enif_make_atom(env, "no_mem");
    
    binson_parser_get_string_copy(&p, string_val);
    ERL_NIF_TERM ret_string = enif_make_string(env, string_val, ERL_NIF_LATIN1);
    free(string_val);
    return ret_string;
}

static ERL_NIF_TERM parser_get_bytes_copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary binary;
    bbuf src_buf;

    binson_tok_size len = binson_parser_get_bytes_len(&p);

    if (!enif_alloc_binary(len, &binary)) return enif_make_atom(env, "no_mem");  
    
    src_buf.bsize = len; src_buf.bptr  = binary.data;
    binson_parser_get_bytes_copy(&p, &src_buf);

    return enif_make_binary(env, &binary);
}
    
static ErlNifFunc nif_funcs[] =
{
    {"ver", 0, ver},
    {"writer_init", 0, writer_init},
    {"writer_reset", 0, writer_reset},
    {"write_boolean", 1, write_boolean},
    {"write_integer", 1, write_integer},
    {"write_double", 1, write_double},
    {"write_string", 1, write_string},
    {"write_bytes", 1, write_bytes},
    {"write_array_begin", 0, write_array_begin},
    {"write_array_end", 0, write_array_end},
    {"write_object_begin", 0, write_object_begin},
    {"write_object_end", 0, write_object_end},
    {"writer_get_counter", 0, writer_get_counter},
    {"writer_get_buf", 0, writer_get_buf},
    {"parser_init", 1, parser_init},
    {"parser_go_into_object", 0, parser_go_into_object},
    {"parser_go_upto_object", 0, parser_go_upto_object},
    {"parser_go_into_array", 0, parser_go_into_array},
    {"parser_go_upto_array", 0, parser_go_upto_array},
    {"parser_field", 1, parser_field},
    {"parser_next_array_value", 0, parser_next_array_value},
    {"parser_get_boolean", 0, parser_get_boolean},
    {"parser_get_integer", 0, parser_get_integer},
    {"parser_get_double", 0, parser_get_double},
    {"parser_get_string_copy", 0, parser_get_string_copy},
    {"parser_get_bytes_copy", 0, parser_get_bytes_copy}
};

ERL_NIF_INIT(binson_nif,nif_funcs,NULL,NULL,NULL,NULL)
