
-module(emqx_offline_cli).

-include_lib("emqx/include/emqx_client.hrl").

-export([cmd/1]).

cmd(["arg1", "arg2"]) ->
    emqx_cli:print("ok");

cmd(_) ->
    emqx_cli:usage([{"cmd arg1 arg2", "cmd demo"}]).
