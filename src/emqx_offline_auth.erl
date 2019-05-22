-module(emqx_offline_auth).

-behaviour(emqx_auth_mod).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_client.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").

-export([init/1, check/3, description/0]).

init(Opts) -> {ok, Opts}.

check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
    io:format("Auth Demo: clientId=~p, username=~p, password=~p Opts=~p~n",
              [ClientId, Username, Password, _Opts]),
    ok.

description() -> "EMQ X Offline Auth Module".

