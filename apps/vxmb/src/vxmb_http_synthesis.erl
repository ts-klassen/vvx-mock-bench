-module(vxmb_http_synthesis).

-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_accepted/2, create_resource/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, '*'}, create_resource}
        ],
        Req,
        State
    }.

create_resource(Req0, State) ->
    EvalId = cowboy_req:binding(eval_id, Req0),
    EngineId = vxmb_http_utils:binding_integer(engine_id, Req0),
    {Payload, Req1} = vxmb_http_utils:read_json_body(Req0),
    SpeakerId = maps:get(<<"speaker_id">>, Payload),
    TaskId = maps:get(<<"task_id">>, Payload),
    ok = vxmb:synthesis(EvalId, EngineId, SpeakerId, TaskId),
    Req = vxmb_http_utils:reply_json(202, #{}, Req1),
    {stop, Req, State}.
