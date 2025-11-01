-module(vxmb_http_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([rest_flow/1]).

all() ->
    [rest_flow].

init_per_suite(Config) ->
    EnvOverrides = set_test_env(),
    ok = ensure_started(inets),
    ok = ensure_vxmb_started(),
    rand:seed(exsss, {321, 654, 987}),
    [{env_overrides, EnvOverrides} | Config].

end_per_suite(Config) ->
    _ = application:stop(vxmb),
    _ = application:stop(inets),
    case lists:keyfind(env_overrides, 1, Config) of
        {env_overrides, Overrides} ->
            restore_env(Overrides);
        false ->
            ok
    end,
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

rest_flow(_Config) ->
    Base = base_url(),
    %% create evaluation
    {201, #{<<"eval_id">> := EvalId, <<"config">> := ConfigMap}} =
        request(post, Base ++ "/evaluations", #{}),
    true = maps:is_key(<<"task_count">>, ConfigMap),
    %% fetch tasks
    {200, #{<<"tasks">> := Tasks}} =
        request(post, Base ++ "/evaluations/" ++ binary_to_list(EvalId) ++ "/tasks", #{}),
    true = length(Tasks) > 0,
    Task = hd(Tasks),
    TaskId = maps:get(<<"task_id">>, Task),
    SpeakerId = maps:get(<<"speaker_id">>, Task),
    %% set speaker
    {202, #{}} =
        request(put,
            Base ++ "/evaluations/" ++ binary_to_list(EvalId) ++ "/engines/0/speaker",
            #{<<"speaker_id">> => SpeakerId}),
    %% synthesis
    {202, #{}} =
        request(post,
            Base ++ "/evaluations/" ++ binary_to_list(EvalId) ++ "/engines/0/synthesis",
            #{<<"speaker_id">> => SpeakerId, <<"task_id">> => TaskId}),
    %% metrics
    {200, #{<<"score">> := Score}} =
        request(get, Base ++ "/evaluations/" ++ binary_to_list(EvalId) ++ "/metrics", undefined),
    true = is_float(Score) orelse is_integer(Score),
    %% config
    {200, ConfigBody} = request(get, Base ++ "/config", undefined),
    true = maps:is_key(<<"task_count">>, ConfigBody),
    ok.

request(Method, Url, Body) ->
    Headers = [{"content-type", "application/json"}],
    Payload = case Body of
        #{} -> jsone:encode(Body);
        undefined -> <<>>;
        _ -> jsone:encode(Body)
    end,
    Request = case Method of
        get ->
            {Url, []};
        _ ->
            {Url, Headers, "application/json", Payload}
    end,
    {ok, {{_, Status, _}, _RespHeaders, RespBody0}} =
        httpc:request(Method, Request, [], []),
    RespBody = to_binary(RespBody0),
    {Status, maybe_decode(Status, RespBody)}.

maybe_decode(204, _) ->
    #{};
maybe_decode(Status, Body) when Status >= 200, Status < 300 ->
    case Body of
        <<>> -> #{};
        _ -> jsone:decode(Body, [{object_format, map}])
    end;
maybe_decode(_, _) ->
    #{}.

base_url() ->
    Port = await_port(),
    "http://localhost:" ++ integer_to_list(Port) ++ "/api/v1".

set_test_env() ->
    Env = [
        {task_count, 2},
        {overhead, 1},
        {engine_count, 1},
        {speaker_count, 2},
        {sleep_min, 1},
        {sleep_max, 2},
        {nbf_uniform, 1},
        {http_port, 0}
    ],
    lists:map(fun({Key, Value}) ->
        Previous = case application:get_env(vxmb, Key) of
            {ok, PrevValue} -> {ok, PrevValue};
            undefined -> undefined
        end,
        ok = application:set_env(vxmb, Key, Value),
        {Key, Previous}
    end, Env).

restore_env(Overrides) ->
    lists:foreach(fun
        ({Key, undefined}) ->
            application:unset_env(vxmb, Key);
        ({Key, {ok, Value}}) ->
            ok = application:set_env(vxmb, Key, Value)
    end, Overrides).

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {App, {{shutdown, {failed_to_start_child, _, {already_started, _}}}, _}}} ->
            ok;
        Other ->
            ct:fail({failed_to_start, App, Other})
    end.

to_binary(Data) when is_binary(Data) ->
    Data;
to_binary(Data) when is_list(Data) ->
    list_to_binary(Data);
to_binary(undefined) ->
    <<>>.

ensure_vxmb_started() ->
    stop_if_running(vxmb),
    stop_listener(vxmb_http),
    case application:ensure_all_started(vxmb) of
        {ok, _} ->
            ok;
        {error, {already_started, vxmb}} ->
            ok;
        Other ->
            ct:fail({failed_to_start, vxmb, Other})
    end,
    case whereis(vxmb) of
        undefined ->
            ct:fail(vxmb_not_running);
        _ ->
            ok
    end.

await_port() ->
    case ranch:get_port(vxmb_http) of
        {ok, Port} when is_integer(Port), Port > 0 ->
            Port;
        Port when is_integer(Port), Port > 0 ->
            Port;
        _ ->
            ct:fail(no_listener_port)
    end.

stop_if_running(App) ->
    case lists:keyfind(App, 1, application:which_applications()) of
        {App, _, _} ->
            _ = application:stop(App),
            ok;
        false ->
            ok
    end.

stop_listener(Name) ->
    case catch cowboy:stop_listener(Name) of
        ok -> ok;
        {error, not_found} -> ok;
        {'EXIT', _} -> ok;
        _ -> ok
    end.
