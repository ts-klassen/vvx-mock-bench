-module(vxmb_http_utils).

-export([reply_json/3, read_json_body/1, config_to_json/1, tasks_to_json/1, binding_integer/2]).

reply_json(Status, Data, Req0) ->
    Body = jsone:encode(Data),
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req0
    ).

read_json_body(Req0) ->
    {Body, Req} = read_full_body(Req0, <<>>),
    {jsone:decode(Body, [{object_format, map}]), Req}.

config_to_json(Config) ->
    maps:from_list([{atom_to_binary(Key, utf8), Value} || {Key, Value} <- maps:to_list(Config)]).

tasks_to_json(Tasks) ->
    lists:map(fun(#{speaker_id := Speaker, payload := Task}) ->
        #{
            <<"task_id">> => Task,
            <<"speaker_id">> => Speaker
        }
    end, Tasks).

binding_integer(Name, Req) ->
    binary_to_integer(cowboy_req:binding(Name, Req)).

read_full_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Chunk, Req} ->
            {<<Acc/binary, Chunk/binary>>, Req};
        {more, Chunk, Req} ->
            read_full_body(Req, <<Acc/binary, Chunk/binary>>)
    end.
