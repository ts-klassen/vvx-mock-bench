-module(vxmb_evaluate_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([evaluate_penalty_reduces_after_completion/1]).

-define(ENGINE_ID, 0).

all() ->
    [evaluate_penalty_reduces_after_completion].

init_per_suite(Config) ->
    EnvOverrides = set_test_env(),
    {ok, _} = application:ensure_all_started(vxmb),
    rand:seed(exsss, {1234, 5678, 9012}),
    [{env_overrides, EnvOverrides} | Config].

end_per_suite(Config) ->
    _ = application:stop(vxmb),
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

evaluate_penalty_reduces_after_completion(_Config) ->
    Id = vxmb:new(),
    timer:sleep(10),
    EvalBefore = vxmb:evaluate(Id),
    true = abs(EvalBefore - 1.0e9) < 1.0e-3,
    Tasks = vxmb:fetch_tasks(Id),
    true = length(Tasks) > 0,
    Task = hd(Tasks),
    SId = maps:get(speaker_id, Task),
    TId = maps:get(payload, Task),
    ok = vxmb:set_engine_speaker(Id, ?ENGINE_ID, SId),
    ok = vxmb:synthesis(Id, ?ENGINE_ID, SId, TId),
    EvalAfter = vxmb:evaluate(Id),
    true = EvalAfter < EvalBefore,
    ok.

set_test_env() ->
    Env = [
        {task_count, 3},
        {overhead, 1},
        {engine_count, 1},
        {speaker_count, 2},
        {sleep_min, 1},
        {sleep_max, 2},
        {nbf_uniform, 1}
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
