%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqx.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_offline).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([load/1, unload/0]).

%% Hooks functions

-export([
    on_message_publish/2,
    on_client_disconnected/3
]).

-define(PUSH_NOTIFICATION_TOPIC, <<"pushnotification">>).

%% Called when the plugin application start
load(Env) ->
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
    emqx:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]).

%% transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};
on_message_publish(Message=#message{from = ?MODULE}, _Env) ->
%%    lager:info("[Offline] Skip own message ~s~n", [Message]),
    {ok, Message};
on_message_publish(Message, _Env) ->
%%    lager:info("[Offline] Processing message ~p", [Message]),
    spawn(fun() ->
      #message{topic = Topic, payload = Payload} = Message,
      case mnesia:dirty_read(emqx_topic, Topic) of
        [] ->
%%          lager:info("[Offline] ~p: Looks like the topic '~s' isn't accessible", [?MODULE, Topic]),
          Message1 = emqx_message:make(?MODULE, ?PUSH_NOTIFICATION_TOPIC, Payload),
          Res = emqx:publish(Message1);
%%          lager:info("[Offline] ~p: Redirecting the message to the topic '~s': ~p", [?MODULE, ?PUSH_NOTIFICATION_TOPIC, Res]);
        [#{}] ->
          ok
      end
    end),
    {ok, Message}.

on_client_disconnected(#{client_id := ClientId, username := Username}, ReasonCode, _Env) ->
    case emqx:lookup_session(ClientId) of
        % undefined ->
            % lager:error("[Offline] @@@Client(~s/~s) session is undefined", [ClientId, Username]);
        Session ->
            State = emqx_session:state(Session#session.pid),
            InFlight = proplists:get_value(inflight, State),
            InFlightMsgs = emqx_inflight:values(InFlight),
            ok = send_not_delivered(InFlightMsgs)
    end.

%% Called when the plugin application stop
unload() ->
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqx:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3).

send_not_delivered([])->
    ok;
send_not_delivered([{_,  #message{payload = Payload} = Msg, _} | InFlight]) ->
    % lager:info("[Offline] InFlightMsg ~p~n", [Msg]),
    Message1 = emqx_message:make(?MODULE, ?PUSH_NOTIFICATION_TOPIC, Payload),
    Res = emqx:publish(Message1),
    % lager:info("[Offline] ~p: Redirecting the message to the topic '~s': ~p", [?MODULE, ?PUSH_NOTIFICATION_TOPIC, Res]),
    send_not_delivered(InFlight).

