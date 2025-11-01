-module(vxmb_http_config).

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

to_json(Req, State) ->
    Body = jsone:encode(vxmb_http_utils:config_to_json(vxmb:config())),
    {Body, Req, State}.
