defmodule Binson do
    @bs_begin       0x40  #64
    @bs_end         0x41  #65
    @bs_begin_arr   0x42  #66
    @bs_end_arr     0x43  #67
    @bs_true        0x44  #68
    @bs_false       0x45  #69
    @bs_double      0x46  #70
    @bs_int8        0x10  #16
    @bs_int16       0x11  #17
    @bs_int32       0x12  #18
    @bs_int64       0x13  #19
    @bs_str_len8    0x14  #20
    @bs_str_len16   0x15  #21
    @bs_str_len32   0x16  #22
    @bs_byte_len8   0x18  #24
    @bs_byte_len16  0x19  #25
    @bs_byte_len32  0x1a  #26

    @bs_bound8     0x80         # 1<<7
    @bs_bound16    0x8000       # 1<<15
    @bs_bound32    0x80000000   # 1<<31

#------ Primitives encoding ------
    def encode(true),  do: <<@bs_true>>
    def encode(false), do: <<@bs_false>>

    def encode(int) when is_integer(int) do
        cond do
            (int >= -@bs_bound8)  and (int < @bs_bound8)  -> <<@bs_int8,  int::size(8)-signed-integer-little>>
            (int >= -@bs_bound16) and (int < @bs_bound16) -> <<@bs_int16, int::size(16)-signed-integer-little>>
            (int >= -@bs_bound32) and (int < @bs_bound32) -> <<@bs_int32, int::size(32)-signed-integer-little>>
            true                                          -> <<@bs_int64, int::size(64)-signed-integer-little>>
        end
    end

    def encode(float) when is_float(float), do: <<@bs_double, float::float-little>>;

    def encode(string) when is_binary(string) do
        len = byte_size(string)
        cond do
            len < @bs_bound8  -> <<@bs_str_len8,  len::size(8)-integer-little,  string::binary>>
            len < @bs_bound16 -> <<@bs_str_len16, len::size(16)-integer-little, string::binary>>
            true              -> <<@bs_str_len32, len::size(32)-integer-little, string::binary>>
        end
    end

    def encode({:bin, bytes}) do
        len = byte_size(bytes)
        cond do
            len < @bs_bound8  -> <<@bs_byte_len8,  len::size(8)-integer-little,  bytes::binary>>
            len < @bs_bound16 -> <<@bs_byte_len16, len::size(16)-integer-little, bytes::binary>>
            true              -> <<@bs_byte_len32, len::size(32)-integer-little, bytes::binary>>
        end
    end

#------ Composites encoding ------
    def encode(array) when is_list(array), do: encode_array(array, <<@bs_begin_arr>>)

    def encode(object) when is_map(object) do
        Map.to_list(object) |> List.keysort(0) |> encode_object(<<@bs_begin>>)
    end

#------ Composites encoding private functions ------
    defp encode_array([], acc), do: <<acc::binary, @bs_end_arr>>
    defp encode_array([value|tail], acc) do
        encode_array(tail, <<acc::binary, encode(value)::binary>>)
    end

    defp encode_object([], acc), do: <<acc::binary, @bs_end>>
    defp encode_object([{key, value}|tail], acc) do
        encode_object(tail, <<acc::binary, encode(key)::binary, encode(value)::binary>>)
    end

#------ Primitives decoding ------
    def decode(<<@bs_true,  rest::binary>>) , do: {true, rest}
    def decode(<<@bs_false, rest::binary>>) , do: {false, rest}

    def decode(<<@bs_int8,  int::size(8)-signed-integer-little,  rest::binary>>), do: {int, rest}
    def decode(<<@bs_int16, int::size(16)-signed-integer-little, rest::binary>>), do: {int, rest}
    def decode(<<@bs_int32, int::size(32)-signed-integer-little, rest::binary>>), do: {int, rest}
    def decode(<<@bs_int64, int::size(64)-signed-integer-little, rest::binary>>), do: {int, rest}

    def decode(<<@bs_double, float::float-little, rest::binary>>), do: {float, rest}

    def decode(<<@bs_str_len8,  len::size(8)-integer-little,  str::size(len)-binary, rest::binary>>), do: {str, rest}
    def decode(<<@bs_str_len16, len::size(16)-integer-little, str::size(len)-binary, rest::binary>>), do: {str, rest}
    def decode(<<@bs_str_len32, len::size(32)-integer-little, str::size(len)-binary, rest::binary>>), do: {str, rest}

    def decode(<<@bs_byte_len8,  len::size(8)-integer-little,  bytes::size(len)-binary, rest::binary>>), do: {{:bin, bytes}, rest}
    def decode(<<@bs_byte_len16, len::size(16)-integer-little, bytes::size(len)-binary, rest::binary>>), do: {{:bin, bytes}, rest}
    def decode(<<@bs_byte_len32, len::size(32)-integer-little, bytes::size(len)-binary, rest::binary>>), do: {{:bin, bytes}, rest}

#------ Composites decoding ------
    def decode(<<@bs_begin_arr, rest::binary>>), do: decode_array(rest, [])
    def decode(<<@bs_begin,     rest::binary>>), do: decode_object(rest, %{})

#------ Composites decoding private functions ------
    defp decode_object(<<@bs_end, rest::binary>>, acc), do: {acc, rest}
    defp decode_object(data, acc) do
        {key, val_rest} = decode(data)
        {value, rest}   = decode(val_rest)
        decode_object(rest, Map.put(acc, key, value))
    end

    defp decode_array(<<@bs_end_arr, rest::binary>>, acc), do: {:lists.reverse(acc), rest}
    defp decode_array(data, acc) do
        {value, rest} = decode(data)
        decode_array(rest, [value|acc])
    end
end
