-module(vxmb_http_evaluations).

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
    EvalId = vxmb:new(),
    Config = vxmb:config(),
    Body = #{
        <<"eval_id">> => EvalId,
        <<"config">> => vxmb_http_utils:config_to_json(Config)
    },
    Req = vxmb_http_utils:reply_json(201, Body, Req0),
    {stop, Req, State}.
