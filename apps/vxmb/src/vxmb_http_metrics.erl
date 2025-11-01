-module(vxmb_http_metrics).

-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, '*'}, to_json}
        ],
        Req,
        State
    }.

to_json(Req0, State) ->
    EvalId = cowboy_req:binding(eval_id, Req0),
    Body = jsone:encode(#{
        <<"score">> => vxmb:evaluate(EvalId)
    }),
    {Body, Req0, State}.
