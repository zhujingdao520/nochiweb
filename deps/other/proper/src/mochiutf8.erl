%% @copyright 2010 Mochi Media, Inc.
%% @author Bob Ippolito <bob@mochimedia.com>

%% @doc Algorithm to convert any binary to a valid UTF-8 sequence by ignoring
%%      invalid bytes.

-module(mochiutf8).
-export([valid_utf8_bytes/1, codepoint_to_bytes/1, codepoints_to_bytes/1]).
-export([bytes_to_codepoints/1, bytes_foldl/3, codepoint_foldl/3]).
-export([read_codepoint/1, len/1]).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

%% External API

-type unichar_low() :: 0..16#d7ff.
-type unichar_high() :: 16#e000..16#10ffff.
-type unichar() :: unichar_low() | unichar_high().
-type utf8_binary() :: binary().
-type nonempty_utf8_binary() :: <<_:8, _:_*8>>.
-type accfun(T, Acc) :: fun((T, Acc) -> Acc).


%% @doc Convert a unicode codepoint to UTF-8 bytes.
-spec codepoint_to_bytes(unichar()) -> nonempty_utf8_binary().
codepoint_to_bytes(C) when (C >= 16#00 andalso C =< 16#7f) ->
    %% U+0000 - U+007F - 7 bits
    <<C>>;
codepoint_to_bytes(C) when (C >= 16#080 andalso C =< 16#07FF) ->
    %% U+0080 - U+07FF - 11 bits
    <<0:5, B1:5, B0:6>> = <<C:16>>,
    <<2#110:3, B1:5,
      2#10:2, B0:6>>;
codepoint_to_bytes(C) when (C >= 16#0800 andalso C =< 16#FFFF) andalso
                           (C < 16#D800 orelse C > 16#DFFF) ->
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    <<B2:4, B1:6, B0:6>> = <<C:16>>,
    <<2#1110:4, B2:4,
      2#10:2, B1:6,
      2#10:2, B0:6>>;
codepoint_to_bytes(C) when (C >= 16#010000 andalso C =< 16#10FFFF) ->
    %% U+10000 - U+10FFFF - 21 bits
    <<0:3, B3:3, B2:6, B1:6, B0:6>> = <<C:24>>,
    <<2#11110:5, B3:3,
      2#10:2, B2:6,
      2#10:2, B1:6,
      2#10:2, B0:6>>.

%% @doc Convert a list of codepoints to a UTF-8 binary.
-spec codepoints_to_bytes([unichar()]) -> utf8_binary().
codepoints_to_bytes(L) ->
    <<<<(codepoint_to_bytes(C))/binary>> || C <- L>>.

-spec read_codepoint(nonempty_utf8_binary()) ->
                            {unichar(), nonempty_utf8_binary(), utf8_binary()}.
read_codepoint(Bin = <<2#0:1, C:7, Rest/binary>>) ->
    %% U+0000 - U+007F - 7 bits
    <<B:1/binary, _/binary>> = Bin,
    {C, B, Rest};
read_codepoint(Bin = <<2#110:3, B1:5,
                       2#10:2, B0:6,
                       Rest/binary>>) ->
    %% U+0080 - U+07FF - 11 bits
    case <<B1:5, B0:6>> of
        <<C:11>> when C >= 16#80 ->
            <<B:2/binary, _/binary>> = Bin,
            {C, B, Rest}
    end;
read_codepoint(Bin = <<2#1110:4, B2:4,
                       2#10:2, B1:6,
                       2#10:2, B0:6,
                       Rest/binary>>) ->
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    case <<B2:4, B1:6, B0:6>> of
        <<C:16>> when (C >= 16#0800 andalso C =< 16#FFFF) andalso
                      (C < 16#D800 orelse C > 16#DFFF) ->
            <<B:3/binary, _/binary>> = Bin,
            {C, B, Rest}
    end;
read_codepoint(Bin = <<2#11110:5, B3:3,
                       2#10:2, B2:6,
                       2#10:2, B1:6,
                       2#10:2, B0:6,
                       Rest/binary>>) ->
    %% U+10000 - U+10FFFF - 21 bits
    case <<B3:3, B2:6, B1:6, B0:6>> of
        <<C:21>> when (C >= 16#010000 andalso C =< 16#10FFFF) ->
            <<B:4/binary, _/binary>> = Bin,
            {C, B, Rest}
    end.

-spec codepoint_foldl(accfun(unichar(), Acc), Acc, utf8_binary()) -> Acc.
codepoint_foldl(F, Acc, <<>>) when is_function(F, 2) ->
    Acc;
codepoint_foldl(F, Acc, Bin) ->
    {C, _, Rest} = read_codepoint(Bin),
    codepoint_foldl(F, F(C, Acc), Rest).

-spec bytes_foldl(accfun(utf8_binary(), Acc), Acc, utf8_binary()) -> Acc.
bytes_foldl(F, Acc, <<>>) when is_function(F, 2) ->
    Acc;
bytes_foldl(F, Acc, Bin) ->
    {_, B, Rest} = read_codepoint(Bin),
    bytes_foldl(F, F(B, Acc), Rest).

-spec bytes_to_codepoints(utf8_binary()) -> [unichar()].
bytes_to_codepoints(B) ->
    lists:reverse(codepoint_foldl(fun (C, Acc) -> [C | Acc] end, [], B)).

-spec len(utf8_binary()) -> non_neg_integer().
len(<<>>) ->
    0;
len(B) ->
    {_, _, Rest} = read_codepoint(B),
    1 + len(Rest).

%% @doc Return only the bytes in B that represent valid UTF-8. Uses
%%      the following recursive algorithm: skip one byte if B does not
%%      follow UTF-8 syntax (a 1-4 byte encoding of some number),
%%      skip sequence of 2-4 bytes if it represents an overlong encoding
%%      or bad code point (surrogate U+D800 - U+DFFF or > U+10FFFF).
-spec valid_utf8_bytes(binary()) -> utf8_binary().
valid_utf8_bytes(B) when is_binary(B) ->
    binary_skip_bytes(B, invalid_utf8_indexes(B)).

%% Internal API

%% @doc Return B, but skipping the 0-based indexes in L.
binary_skip_bytes(B, []) ->
    B;
binary_skip_bytes(B, L) ->
    binary_skip_bytes(B, L, 0, []).

%% @private
binary_skip_bytes(B, [], _N, Acc) ->
    iolist_to_binary(lists:reverse([B | Acc]));
binary_skip_bytes(<<_, RestB/binary>>, [N | RestL], N, Acc) ->
    binary_skip_bytes(RestB, RestL, 1 + N, Acc);
binary_skip_bytes(<<C, RestB/binary>>, L, N, Acc) ->
    binary_skip_bytes(RestB, L, 1 + N, [C | Acc]).

%% @doc Return the 0-based indexes in B that are not valid UTF-8.
invalid_utf8_indexes(B) ->
    invalid_utf8_indexes(B, 0, []).

%% @private.
invalid_utf8_indexes(<<C, Rest/binary>>, N, Acc) when C < 16#80 ->
    %% U+0000 - U+007F - 7 bits
    invalid_utf8_indexes(Rest, 1 + N, Acc);
invalid_utf8_indexes(<<C1, C2, Rest/binary>>, N, Acc)
  when C1 band 16#E0 =:= 16#C0,
       C2 band 16#C0 =:= 16#80 ->
    %% U+0080 - U+07FF - 11 bits
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when C < 16#80 ->
            %% Overlong encoding.
            invalid_utf8_indexes(Rest, 2 + N, [1 + N, N | Acc]);
        _ ->
            %% Upper bound U+07FF does not need to be checked
            invalid_utf8_indexes(Rest, 2 + N, Acc)
    end;
invalid_utf8_indexes(<<C1, C2, C3, Rest/binary>>, N, Acc)
  when C1 band 16#F0 =:= 16#E0,
       C2 band 16#C0 =:= 16#80,
       C3 band 16#C0 =:= 16#80 ->
    %% U+0800 - U+FFFF - 16 bits
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when (C < 16#800) orelse (C >= 16#D800 andalso C =< 16#DFFF) ->
	    %% Overlong encoding or surrogate.
            invalid_utf8_indexes(Rest, 3 + N, [2 + N, 1 + N, N | Acc]);
	_ ->
            %% Upper bound U+FFFF does not need to be checked
	    invalid_utf8_indexes(Rest, 3 + N, Acc)
    end;
invalid_utf8_indexes(<<C1, C2, C3, C4, Rest/binary>>, N, Acc)
  when C1 band 16#F8 =:= 16#F0,
       C2 band 16#C0 =:= 16#80,
       C3 band 16#C0 =:= 16#80,
       C4 band 16#C0 =:= 16#80 ->
    %% U+10000 - U+10FFFF - 21 bits
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
           (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when (C < 16#10000) orelse (C > 16#10FFFF) ->
	    %% Overlong encoding or invalid code point.
	    invalid_utf8_indexes(Rest, 4 + N, [3 + N, 2 + N, 1 + N, N | Acc]);
	_ ->
	    invalid_utf8_indexes(Rest, 4 + N, Acc)
    end;
invalid_utf8_indexes(<<_, Rest/binary>>, N, Acc) ->
    %% Invalid char
    invalid_utf8_indexes(Rest, 1 + N, [N | Acc]);
invalid_utf8_indexes(<<>>, _N, Acc) ->
    lists:reverse(Acc).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-ifdef(PROPER).
utf8_binary() ->
    ?LET(L, list(unichar()),
         unicode:characters_to_binary(L, utf8)).

nonempty_utf8_binary() ->
    ?LET(L, [unichar() | list(unichar())],
         unicode:characters_to_binary(L, utf8)).

prop_valid_utf8_bytes_valid() ->
    ?FORALL(B, utf8_binary(),
            B =:= valid_utf8_bytes(B)).

prop_valid_utf8_bytes_garbage() ->
    ?FORALL(B, binary(),
            begin
                ValidB = valid_utf8_bytes(B),
                (byte_size(ValidB) =< byte_size(B) andalso
                 ValidB =:= valid_utf8_bytes(ValidB) andalso
                 ValidB =:= unicode:characters_to_binary(
                              unicode:characters_to_list(ValidB, utf8),
                              utf8))
            end).

prop_codepoint_to_bytes_verify() ->
    ?FORALL(C, unichar(),
            codepoint_to_bytes(C) =:=
                unicode:characters_to_binary([C], utf8)).

prop_codepoints_to_bytes_verify() ->
    ?FORALL(L, [unichar()],
            codepoints_to_bytes(L) =:=
                unicode:characters_to_binary(L, utf8)).

prop_bytes_to_codepoints_verify() ->
    ?FORALL(B, utf8_binary(),
            bytes_to_codepoints(B) =:= unicode:characters_to_list(B, utf8)).

prop_bytes_foldl_verify() ->
    ?FORALL(B, utf8_binary(),
            begin
                F = fun (C, Acc) -> [C | Acc] end,
                lists:reverse(bytes_foldl(F, [], B)) =:=
                    [codepoint_to_bytes(C) || C <- bytes_to_codepoints(B)]
            end).

prop_codepoint_foldl_verify() ->
    ?FORALL(B, utf8_binary(),
            begin
                F = fun (C, Acc) -> [C | Acc] end,
                lists:reverse(codepoint_foldl(F, [], B)) =:=
                    bytes_to_codepoints(B)
            end).

prop_read_codepoint_verify() ->
    ?FORALL(B, nonempty_utf8_binary(),
            begin
                {Codepoint, Char, Rest} = read_codepoint(B),
                L = unicode:characters_to_list(B, utf8),
                (Codepoint =:= hd(L) andalso
                 Char =:= codepoint_to_bytes(Codepoint) andalso
                 Rest =:= codepoints_to_bytes(tl(L)))
            end).

prop_len_verify() ->
    ?FORALL(B, utf8_binary(),
            len(B) =:= length(bytes_to_codepoints(B))).

-endif.
%% Can't use the spec tests because of utf8_binary(), nonempty_utf8_binary()
%% and accfun(T,Acc).
-define(PROPER_NO_SPECS, true).
-include("proper_tests.hrl").

binary_skip_bytes_test() ->
    ?assertEqual(<<"foo">>,
                 binary_skip_bytes(<<"foo">>, [])),
    ?assertEqual(<<"foobar">>,
                 binary_skip_bytes(<<"foo bar">>, [3])),
    ?assertEqual(<<"foo">>,
                 binary_skip_bytes(<<"foo bar">>, [3, 4, 5, 6])),
    ?assertEqual(<<"oo bar">>,
                 binary_skip_bytes(<<"foo bar">>, [0])),
    ok.

invalid_utf8_indexes_test() ->
    ?assertEqual(
       [],
       invalid_utf8_indexes(<<"unicode snowman for you: ", 226, 152, 131>>)),
    ?assertEqual(
       [0],
       invalid_utf8_indexes(<<128>>)),
    ?assertEqual(
       [57,59,60,64,66,67],
       invalid_utf8_indexes(<<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (",
                              167, 65, 170, 186, 73, 83, 80, 166, 87, 186, 217, 41, 41>>)),
    ok.

codepoint_to_bytes_test() ->
    %% U+0000 - U+007F - 7 bits
    %% U+0080 - U+07FF - 11 bits
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    %% U+10000 - U+10FFFF - 21 bits
    ?assertEqual(
       <<"a">>,
       codepoint_to_bytes($a)),
    ?assertEqual(
       <<16#c2, 16#80>>,
       codepoint_to_bytes(16#80)),
    ?assertEqual(
       <<16#df, 16#bf>>,
       codepoint_to_bytes(16#07ff)),
    ?assertEqual(
       <<16#ef, 16#bf, 16#bf>>,
       codepoint_to_bytes(16#ffff)),
    ?assertEqual(
       <<16#f4, 16#8f, 16#bf, 16#bf>>,
       codepoint_to_bytes(16#10ffff)),
    ok.

bytes_foldl_test() ->
    ?assertEqual(
       <<"abc">>,
       bytes_foldl(fun (B, Acc) -> <<Acc/binary, B/binary>> end, <<>>, <<"abc">>)),
    ?assertEqual(
       <<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>,
       bytes_foldl(fun (B, Acc) -> <<Acc/binary, B/binary>> end, <<>>,
                   <<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>)),
    ok.

bytes_to_codepoints_test() ->
    ?assertEqual(
       "abc" ++ [16#2603, 16#4e2d, 16#85, 16#10ffff],
       bytes_to_codepoints(<<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>)),
    ok.

codepoint_foldl_test() ->
    ?assertEqual(
       "cba",
       codepoint_foldl(fun (C, Acc) -> [C | Acc] end, [], <<"abc">>)),
    ?assertEqual(
       [16#10ffff, 16#85, 16#4e2d, 16#2603 | "cba"],
       codepoint_foldl(fun (C, Acc) -> [C | Acc] end, [],
                       <<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>)),
    ok.

len_test() ->
    ?assertEqual(
       29,
       len(<<"unicode snowman for you: ", 226, 152, 131, 228, 184, 173, 194, 133, 244, 143, 191, 191>>)),
    ok.

codepoints_to_bytes_test() ->
    ?assertEqual(
       iolist_to_binary(lists:map(fun codepoint_to_bytes/1, lists:seq(1, 1000))),
       codepoints_to_bytes(lists:seq(1, 1000))),
    ok.

valid_utf8_bytes_test() ->
    ?assertEqual(
       <<"invalid U+11ffff: ">>,
       valid_utf8_bytes(<<"invalid U+11ffff: ", 244, 159, 191, 191>>)),
    ?assertEqual(
       <<"U+10ffff: ", 244, 143, 191, 191>>,
       valid_utf8_bytes(<<"U+10ffff: ", 244, 143, 191, 191>>)),
    ?assertEqual(
       <<"overlong 2-byte encoding (a): ">>,
       valid_utf8_bytes(<<"overlong 2-byte encoding (a): ", 2#11000001, 2#10100001>>)),
    ?assertEqual(
       <<"overlong 2-byte encoding (!): ">>,
       valid_utf8_bytes(<<"overlong 2-byte encoding (!): ", 2#11000000, 2#10100001>>)),
    ?assertEqual(
       <<"mu: ", 194, 181>>,
       valid_utf8_bytes(<<"mu: ", 194, 181>>)),
    ?assertEqual(
       <<"bad coding bytes: ">>,
       valid_utf8_bytes(<<"bad coding bytes: ", 2#10011111, 2#10111111, 2#11111111>>)),
    ?assertEqual(
       <<"low surrogate (unpaired): ">>,
       valid_utf8_bytes(<<"low surrogate (unpaired): ", 237, 176, 128>>)),
    ?assertEqual(
       <<"high surrogate (unpaired): ">>,
       valid_utf8_bytes(<<"high surrogate (unpaired): ", 237, 191, 191>>)),
    ?assertEqual(
       <<"unicode snowman for you: ", 226, 152, 131>>,
       valid_utf8_bytes(<<"unicode snowman for you: ", 226, 152, 131>>)),
    ?assertEqual(
       <<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (AISPW))">>,
       valid_utf8_bytes(<<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (",
                          167, 65, 170, 186, 73, 83, 80, 166, 87, 186, 217, 41, 41>>)),
    ok.

-endif.
