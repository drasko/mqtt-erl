-module(mfx_auth_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    % Put ENV variables in ETS
    ets:new(mfx_cfg, [set, named_table]),

    NatsUrl = case os:getenv("MF_NATS_URL") of
        false -> "nats://localhost:4222";
        NatsEnv -> NatsEnv
    end,

    ThingsUrl = case os:getenv("MF_THINGS_URL") of
        false -> "http://localhost:8181";
        ThingsEnv -> ThingsEnv
    end,

    {ok, {_, _, NatsHost, NatsPort, _, _}} = http_uri:parse(NatsUrl),
    {ok, NatsConn} = nats:connect(list_to_binary(NatsHost), NatsPort, #{buffer_size => 10}),

    ets:insert(mfx_cfg, [
        {nats_url, NatsUrl},
        {things_url, ThingsUrl},
        {nats_conn, NatsConn}
    ]),

    % Start the process
    mfx_auth_sup:start_link().

stop(_State) ->
    ok.
