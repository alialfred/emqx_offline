
-module(emqx_offline_cli).

-include_lib("emqx/include/emqx_client.hrl").
-include_lib("emqx/include/logger.hrl").

-export([cmd/1]).

cmd(["arg1", "arg2"]) ->
    ?PRINT_MSG("ok");

cmd(_) ->
    ?USAGE([{"cmd arg1 arg2", "cmd demo"}]).
