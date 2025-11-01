-module(vxmb_http).

-export([start_link/0]).

-define(DEFAULT_PORT, 8080).

start_link() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/evaluations", vxmb_http_evaluations, #{}},
            {"/api/v1/evaluations/:eval_id/tasks", vxmb_http_tasks, #{}},
            {"/api/v1/evaluations/:eval_id/engines/:engine_id/speaker", vxmb_http_engine_speaker, #{}},
            {"/api/v1/evaluations/:eval_id/engines/:engine_id/synthesis", vxmb_http_synthesis, #{}},
            {"/api/v1/evaluations/:eval_id/metrics", vxmb_http_metrics, #{}},
            {"/api/v1/config", vxmb_http_config, #{}}
        ]}
    ]),
    Port = application:get_env(vxmb, http_port, ?DEFAULT_PORT),
    cowboy:start_clear(
        ?MODULE,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).
