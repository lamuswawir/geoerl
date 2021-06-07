%%%-------------------------------------------------------------------
%% @doc geoerl public API
%% @end
%%%-------------------------------------------------------------------

-module(geoerl_app).

-behaviour(application).
-include("../include/geo.hrl").

-export([start/2, stop/1]).
-export([all/1, fetch/2]).

start(_StartType, _StartArgs) ->
    inets:start(),
    geoerl_sup:start_link().

stop(_State) ->
    ok.

all(IP) ->
    case httpc:request("http://api.ipstack.com/"++IP++"?access_key="++?AccessKey) of
        {ok, Data} -> build_geo_tuple(Data);
        {error, Reason} -> {error, Reason}
    end.

fetch(IP, List) ->
    case all(IP) of
        {error, Reason} -> {error, Reason};
        Data -> get_required(List, Data, [])
    end.

%% internal functions

build_geo_tuple({{_, 200, "OK"}, _Headers, String}) ->
    {ok, {struct, Data}} = json2:decode_string(String),
    Data.

get_required([], _Data, Acc) ->
    lists:reverse(Acc);
get_required(["languages"|List], Data, Acc) ->
    case proplists:get_value("location", Data, undefined) of
        undefined -> get_required(List, Data, [undefined|Acc]);
        {struct, LocData} ->
            case proplists:get_value("languages", LocData) of
                undefined -> get_required(List, Data, [undefined|Acc]);
                {array, Languages} -> LangList = [L||{struct, [_, {_, L}, _]}<-Languages],
                                      get_required(List, Data, [{"languages", LangList}|Acc])
            end
    end;
get_required(["calling_code"|List], Data, Acc) ->
     case proplists:get_value("location", Data, undefined) of
        undefined -> get_required(List, Data, [undefined|Acc]);
        {struct, LocData} ->
            case proplists:get_value("country_code", LocData) of
                undefined -> get_required(List, Data, [undefined|Acc]);
                Code -> get_required(List, Data, [Code|Acc])
            end
    end;
get_required([Elem|List], Data, Acc) ->
    get_required(List, Data, [proplists:get_value(Elem, Data, undefined)|Acc]).
