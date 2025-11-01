-module(vxmb_http_tasks).

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
    Tasks = vxmb:fetch_tasks(EvalId),
    Body = #{
        <<"tasks">> => vxmb_http_utils:tasks_to_json(Tasks)
    },
    Req = vxmb_http_utils:reply_json(200, Body, Req0),
    {stop, Req, State}.
