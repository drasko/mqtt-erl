-module(mfx_auth).

-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

-include("message.hrl").

%% This file demonstrates the hooks you typically want to use
%% if your plugin deals with Authentication or Authorization.
%%
%% All it does is:
%%  - authenticate every user and write the log
%%  - authorize every PUBLISH and SUBSCRIBE and write it to the log
%%
%% You don't need to implement all of these hooks, just the one
%% needed for your use case.
%%
%% IMPORTANT:
%%  these hook functions run in the session context


publish_to_nats(Subject, Message) ->
    error_logger:info_msg("publish_to_nats: ~p ~p", [Subject, Message]),
    % Publish the message
    [{nats_conn, Conn}] = ets:lookup(mfx_cfg, nats_conn),
    nats:pub(Conn, Subject, #{payload => Message}).

call_grpc(Method, Payload) ->
    [{grpc_conn, Conn}] = ets:lookup(mfx_cfg, grpc_conn),
    {Status, Result} = case Method of
        identify ->
            internal_client:'IdentifyThing'(Conn, Payload, []);
        can_access ->
            internal_client:'CanAccess'(Conn, Payload, []);
        _ ->
            {error, wrong_method}
    end,

    case Status of
        ok ->
            #{
                grpc_status := 0,
                headers := #{<<":status">> := <<"200">>},
                http_status := HttpStatus,
                result :=
                    #{value := ThingId},
                status_message := <<>>,
                trailers := #{<<"grpc-status">> := <<"0">>}
            } = Result,

            case HttpStatus of
                200 ->
                    {ok, ThingId};
                _ ->
                    {error, HttpStatus}
            end;
        _ ->
            {error, Status}
    end.

auth_thing(undefined) ->
    {error, undefined};
auth_thing(Password) ->
    error_logger:info_msg("auth_thing: ~p", [Password]),
    Token = #{value => binary_to_list(Password)},
    call_grpc(identify, Token).
auth_thing(UserName, ChannelId) ->
    error_logger:info_msg("auth_thing: ~p ~p", [UserName, ChannelId]),
    Password = get(UserName),
    case Password of
        undefined ->
            {error, undefined};
        _ ->
            AccessReq = #{token => binary_to_list(Password), chanID => ChannelId},
            call_grpc(can_access, AccessReq)
    end.

auth_on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    error_logger:info_msg("auth_on_register: ~p ~p ~p ~p ~p", [Peer, SubscriberId, UserName, Password, CleanSession]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> CONNECT is authenticated
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {ok, [{ModifierKey, NewVal}...]} -> CONNECT is authenticated, but we might want to set some options used throughout the client session:
    %%      - {mountpoint, NewMountPoint::string}
    %%      - {clean_session, NewCleanSession::boolean}
    %% 4. return {error, invalid_credentials} -> CONNACK_CREDENTIALS is sent
    %% 5. return {error, whatever} -> CONNACK_AUTH is sent

    case auth_thing(Password) of
        {ok, _} ->
            % Save Username:Password mapping in process dictionary
            put(UserName, Password),
            ok;
        _ ->
            error
    end.

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    error_logger:info_msg("auth_on_publish: ~p ~p ~p ~p ~p ~p", [UserName, SubscriberId, QoS, Topic, Payload, IsRetain]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> PUBLISH is authorized
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {ok, NewPayload::binary} -> PUBLISH is authorized, but we changed the payload
    %% 4. return {ok, [{ModifierKey, NewVal}...]} -> PUBLISH is authorized, but we might have changed different Publish Options:
    %%     - {topic, NewTopic::string}
    %%     - {payload, NewPayload::binary}
    %%     - {qos, NewQoS::0..2}
    %%     - {retain, NewRetainFlag::boolean}
    %% 5. return {error, whatever} -> auth chain is stopped, and message is silently dropped (unless it is a Last Will message)
    %%
    [_, ChannelIdBin, _] = Topic,
    ChannelId = binary_to_integer(ChannelIdBin),
    case auth_thing(UserName, ChannelId) of
        {ok, PublisherId} ->
            % Topic is list of binaries, ex: [<<"channels">>,<<"1">>,<<"messages">>]
            Subject = [<<"channel.">>, ChannelId], % binary concatenation
            RawMessage = #'RawMessage'{
                'Channel' = ChannelId,
                'Publisher' = PublisherId,
                'Protocol' = "mqtt",
                'Payload' = Payload
            },

            publish_to_nats(Subject, message:encode_msg(RawMessage)),

            %% we return 'ok'
            ok;
        _ ->
            error
    end.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->
    error_logger:info_msg("auth_on_subscribe: ~p ~p ~p", [UserName, ClientId, Topics]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> SUBSCRIBE is authorized
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {error, whatever} -> auth chain is stopped, and no SUBACK is sent

    [_, ChannelIdBin, _] = _Topic,
    ChannelId = binary_to_integer(ChannelIdBin),
    case auth_thing(UserName, ChannelId) of
        {ok, _} -> ok;
        _ -> error
    end.
