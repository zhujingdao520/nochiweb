%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Case preserving (but case insensitive) HTTP Header dictionary.

-module(mochiweb_headers).
-author('bob@mochimedia.com').
-export([empty/0, from_list/1, insert/3, enter/3, get_value/2, lookup/2]).
-export([delete_any/2, get_primary_value/2]).
-export([default/3, enter_from_list/2, default_from_list/2]).
-export([to_list/1, make/1]).
-export([from_binary/1]).

%% @type headers().
%% @type key() = atom() | binary() | string().
%% @type value() = atom() | binary() | string() | integer().

%% @spec empty() -> headers()
%% @doc Create an empty headers structure.
empty() ->
    {mochiweb_headers, []}.

%% @spec make(headers() | [{key(), value()}]) -> headers()
%% @doc Construct a headers() from the given list.
make(L) when is_list(L) ->
    from_list(L);
%% assume a tuple is already mochiweb_headers.
make(T) when is_tuple(T) ->
    T.

%% @spec from_binary(iolist()) -> headers()
%% @doc Transforms a raw HTTP header into a mochiweb headers structure.
%%
%%      The given raw HTTP header can be one of the following:
%%
%%      1) A string or a binary representing a full HTTP header ending with
%%         double CRLF.
%%         Examples:
%%         ```
%%         "Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n"
%%         <<"Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n">>'''
%%
%%      2) A list of binaries or strings where each element represents a raw
%%         HTTP header line ending with a single CRLF.
%%         Examples:
%%         ```
%%         [<<"Content-Length: 47\r\n">>, <<"Content-Type: text/plain\r\n">>]
%%         ["Content-Length: 47\r\n", "Content-Type: text/plain\r\n"]
%%         ["Content-Length: 47\r\n", <<"Content-Type: text/plain\r\n">>]'''
%%
from_binary(RawHttpHeader) when is_binary(RawHttpHeader) ->
    from_binary(RawHttpHeader, []);
from_binary(RawHttpHeaderList) ->
    from_binary(list_to_binary([RawHttpHeaderList, "\r\n"])).

from_binary(RawHttpHeader, Acc) ->
    case erlang:decode_packet(httph, RawHttpHeader, []) of
        {ok, {http_header, _, H, _, V}, Rest} ->
            from_binary(Rest, [{H, V} | Acc]);
        _ ->
            make(Acc)
    end.

%% @spec from_list([{key(), value()}]) -> headers()
%% @doc Construct a headers() from the given list.
from_list(List) ->
    lists:foldl(fun ({K, V}, T) -> insert(K, V, T) end, empty(), List).

%% @spec enter_from_list([{key(), value()}], headers()) -> headers()
%% @doc Insert pairs into the headers, replace any values for existing keys.
enter_from_list(List, T) ->
    lists:foldl(fun ({K, V}, T1) -> enter(K, V, T1) end, T, List).

%% @spec default_from_list([{key(), value()}], headers()) -> headers()
%% @doc Insert pairs into the headers for keys that do not already exist.
default_from_list(List, T) ->
    lists:foldl(fun ({K, V}, T1) -> default(K, V, T1) end, T, List).

%% @spec to_list(headers()) -> [{key(), string()}]
%% @doc Return the contents of the headers. The keys will be the exact key
%%      that was first inserted (e.g. may be an atom or binary, case is
%%      preserved).
to_list({mochiweb_headers, TL}) ->
    F = fun ({_NK, K, {array, L}}, Acc) ->
                L1 = lists:reverse(L),
                lists:foldl(fun (V, Acc1) -> [{K, V} | Acc1] end, Acc, L1);
            ({_NK, K, V}, Acc) ->
                [{K, V} | Acc]
        end,
    lists:reverse(lists:foldl(F, [], lists:sort(TL))).

%% @spec get_value(key(), headers()) -> string() | undefined
%% @doc Return the value of the given header using a case insensitive search.
%%      undefined will be returned for keys that are not present.
get_value(K, {mochiweb_headers, TL}) when is_atom(K) ->
    case lists:keyfind(K, 2, TL) of
        false ->
            case lists:keyfind(normalize(K), 1, TL) of
                false ->
                    undefined;
                {_NK, _K, V} ->
                    expand(V)
            end;
        {_NK, _K, V} ->
            expand(V)
    end;
get_value(K, {mochiweb_headers, TL}) ->
    case lists:keyfind(normalize(K), 1, TL) of
        false ->
            undefined;
        {_NK, _K, V} ->
            expand(V)
    end.

%% @spec get_primary_value(key(), headers()) -> string() | undefined
%% @doc Return the value of the given header up to the first semicolon using
%%      a case insensitive search. undefined will be returned for keys
%%      that are not present.
get_primary_value(K, T) ->
    case get_value(K, T) of
        undefined ->
            undefined;
        V ->
            lists:takewhile(fun (C) -> C =/= $; end, V)
    end.

%% @spec lookup(key(), headers()) -> {value, {key(), string()}} | none
%% @doc Return the case preserved key and value for the given header using
%%      a case insensitive search. none will be returned for keys that are
%%      not present.
lookup(K, {mochiweb_headers, TL}) when is_atom(K) ->
    case lists:keyfind(K, 2, TL) of
        {_NK, _K, V} ->
            {value, {K, expand(V)}};
        false ->
            case lists:keyfind(normalize(K), 1, TL) of
                {_NK, K0, V} ->
                    {value, {K0, expand(V)}};
                false ->
                    none
            end
    end;
lookup(K, {mochiweb_headers, TL}) ->
    case lists:keyfind(normalize(K), 1, TL) of
        {_NK, K0, V} ->
            {value, {K0, expand(V)}};
        false ->
            none
    end.

%% @spec default(key(), value(), headers()) -> headers()
%% @doc Insert the pair into the headers if it does not already exist.
default(K, V, T={mochiweb_headers, TL}) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    case lists:keymember(K1, 1, TL) of
        false ->
            {mochiweb_headers, [{K1, K, V1} | TL]};
        true ->
            T
    end.

%% @spec enter(key(), value(), headers()) -> headers()
%% @doc Insert the pair into the headers, replacing any pre-existing key.
enter(K, V, {mochiweb_headers, TL}) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    {mochiweb_headers, lists:keystore(K1, 1, TL, {K1, K, V1})}.

%% @spec insert(key(), value(), headers()) -> headers()
%% @doc Insert the pair into the headers, merging with any pre-existing key.
%%      A merge is done with Value = V0 ++ ", " ++ V1.
insert(K, V, {mochiweb_headers, TL}) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    {mochiweb_headers, insert2(K1, TL, K, V1)}.

insert2(Key, [{Key, K0, V0} | TL], _K1, V1) ->
    [{Key, K0, merge(Key, V1, V0)} | TL];
insert2(Key, [H | TL], K1, V1) ->
    [H | insert2(Key, TL, K1, V1)];
insert2(Key, [], K1, V1) ->
    [{Key, K1, V1}].

%% @spec delete_any(key(), headers()) -> headers()
%% @doc Delete the header corresponding to key if it is present.
delete_any(K, {mochiweb_headers, TL}) ->
    K1 = normalize(K),
    {mochiweb_headers, lists:keydelete(K1, 1, TL)}.

%% Internal API

expand({array, L}) ->
    string:join(lists:reverse(L), ", ");
expand(V) ->
    V.

merge("set-cookie", V1, {array, L}) ->
    {array, [V1 | L]};
merge("set-cookie", V1, V0) ->
    {array, [V1, V0]};
merge(_, V1, V0) ->
    V0 ++ ", " ++ V1.

normalize(K) when is_list(K) ->
    string:to_lower(K);
normalize(K) when is_binary(K) ->
    normalize(binary_to_list(K));
normalize('Cache-Control') -> "cache-control";
normalize('Connection') -> "connection";
normalize('Date') -> "date";
normalize('Pragma') -> "pragma";
normalize('Transfer-Encoding') -> "transfer-encoding";
normalize('Upgrade') -> "upgrade";
normalize('Via') -> "via";
normalize('Accept') -> "accept";
normalize('Accept-Charset') -> "accept-charset";
normalize('Accept-Encoding') -> "accept-encoding";
normalize('Accept-Language') -> "accept-language";
normalize('Authorization') -> "authorization";
normalize('From') -> "from";
normalize('Host') -> "host";
normalize('If-Modified-Since') -> "if-modified-since";
normalize('If-Match') -> "if-match";
normalize('If-None-Match') -> "if-none-match";
normalize('If-Range') -> "if-range";
normalize('If-Unmodified-Since') -> "if-unmodified-since";
normalize('Max-Forwards') -> "max-forwards";
normalize('Proxy-Authorization') -> "proxy-authorization";
normalize('Range') -> "range";
normalize('Referer') -> "referer";
normalize('User-Agent') -> "user-agent";
normalize('Age') -> "age";
normalize('Location') -> "location";
normalize('Proxy-Authenticate') -> "proxy-authenticate";
normalize('Public') -> "public";
normalize('Retry-After') -> "retry-after";
normalize('Server') -> "server";
normalize('Vary') -> "vary";
normalize('Warning') -> "warning";
normalize('Www-Authenticate') -> "www-authenticate";
normalize('Allow') -> "allow";
normalize('Content-Base') -> "content-base";
normalize('Content-Encoding') -> "content-encoding";
normalize('Content-Language') -> "content-language";
normalize('Content-Length') -> "content-length";
normalize('Content-Location') -> "content-location";
normalize('Content-Md5') -> "content-md5";
normalize('Content-Range') -> "content-range";
normalize('Content-Type') -> "content-type";
normalize('Etag') -> "etag";
normalize('Expires') -> "expires";
normalize('Last-Modified') -> "last-modified";
normalize('Accept-Ranges') -> "accept-ranges";
normalize('Set-Cookie') -> "set-cookie";
normalize('Set-Cookie2') -> "set-cookie2";
normalize('X-Forwarded-For') -> "x-forwarded-for";
normalize('Cookie') -> "cookie";
normalize('Keep-Alive') -> "keep-alive";
normalize('Proxy-Connection') -> "proxy-connection";
normalize(K) when is_atom(K) ->
    normalize(atom_to_list(K)).

any_to_list(V) when is_list(V) ->
    V;
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V).

%%
%% Tests.
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

make_test() ->
    Identity = make([{hdr, foo}]),
    ?assertEqual(
       Identity,
       make(Identity)).

enter_from_list_test() ->
    H = make([{hdr, foo}]),
    ?assertEqual(
       [{baz, "wibble"}, {hdr, "foo"}],
       to_list(enter_from_list([{baz, wibble}], H))),
    ?assertEqual(
       [{hdr, "bar"}],
       to_list(enter_from_list([{hdr, bar}], H))),
    ok.

default_from_list_test() ->
    H = make([{hdr, foo}]),
    ?assertEqual(
       [{baz, "wibble"}, {hdr, "foo"}],
       to_list(default_from_list([{baz, wibble}], H))),
    ?assertEqual(
       [{hdr, "foo"}],
       to_list(default_from_list([{hdr, bar}], H))),
    ok.

get_primary_value_test() ->
    H = make([{hdr, foo}, {baz, <<"wibble;taco">>}]),
    ?assertEqual(
       "foo",
       get_primary_value(hdr, H)),
    ?assertEqual(
       undefined,
       get_primary_value(bar, H)),
    ?assertEqual(
       "wibble",
       get_primary_value(<<"baz">>, H)),
    ok.

set_cookie_test() ->
    H = make([{"set-cookie", foo}, {"set-cookie", bar}, {"set-cookie", baz}]),
    ?assertEqual(
       [{"set-cookie", "foo"}, {"set-cookie", "bar"}, {"set-cookie", "baz"}],
       to_list(H)),
    ok.

headers_test() ->
    H = ?MODULE:make([{hdr, foo}, {"Hdr", "bar"}, {'Hdr', 2}]),
    [{hdr, "foo, bar, 2"}] = ?MODULE:to_list(H),
    H1 = ?MODULE:insert(taco, grande, H),
    [{hdr, "foo, bar, 2"}, {taco, "grande"}] = ?MODULE:to_list(H1),
    H2 = ?MODULE:make([{"Set-Cookie", "foo"}]),
    [{"Set-Cookie", "foo"}] = ?MODULE:to_list(H2),
    H3 = ?MODULE:insert("Set-Cookie", "bar", H2),
    [{"Set-Cookie", "foo"}, {"Set-Cookie", "bar"}] = ?MODULE:to_list(H3),
    "foo, bar" = ?MODULE:get_value("set-cookie", H3),
    {value, {"Set-Cookie", "foo, bar"}} = ?MODULE:lookup("set-cookie", H3),
    undefined = ?MODULE:get_value("shibby", H3),
    none = ?MODULE:lookup("shibby", H3),
    H4 = ?MODULE:insert("content-type",
                        "application/x-www-form-urlencoded; charset=utf8",
                        H3),
    "application/x-www-form-urlencoded" = ?MODULE:get_primary_value(
                                             "content-type", H4),
    H4 = ?MODULE:delete_any("nonexistent-header", H4),
    H3 = ?MODULE:delete_any("content-type", H4),
    HB = <<"Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n">>,
    H_HB = ?MODULE:from_binary(HB),
    H_HB = ?MODULE:from_binary(binary_to_list(HB)),
    "47" = ?MODULE:get_value("Content-Length", H_HB),
    "text/plain" = ?MODULE:get_value("Content-Type", H_HB),
    L_H_HB = ?MODULE:to_list(H_HB),
    2 = length(L_H_HB),
    true = lists:member({'Content-Length', "47"}, L_H_HB),
    true = lists:member({'Content-Type', "text/plain"}, L_H_HB),
    HL = [ <<"Content-Length: 47\r\n">>, <<"Content-Type: text/plain\r\n">> ],
    HL2 = [ "Content-Length: 47\r\n", <<"Content-Type: text/plain\r\n">> ],
    HL3 = [ <<"Content-Length: 47\r\n">>, "Content-Type: text/plain\r\n" ],
    H_HL = ?MODULE:from_binary(HL),
    H_HL = ?MODULE:from_binary(HL2),
    H_HL = ?MODULE:from_binary(HL3),
    "47" = ?MODULE:get_value("Content-Length", H_HL),
    "text/plain" = ?MODULE:get_value("Content-Type", H_HL),
    L_H_HL = ?MODULE:to_list(H_HL),
    2 = length(L_H_HL),
    true = lists:member({'Content-Length', "47"}, L_H_HL),
    true = lists:member({'Content-Type', "text/plain"}, L_H_HL),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<>>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"\r\n">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"\r\n\r\n">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary("")),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<>>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"">>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"\r\n">>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"\r\n\r\n">>])),
    ok.

-endif.
