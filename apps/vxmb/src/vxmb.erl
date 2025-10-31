-module(vxmb).

-behaviour(gen_server).

-export([
        new/0
      , fetch_tasks/1
      , set_engine_speaker/3
      , synthesis/4
      , config/0
      , evaluate/1
    ]).

-export_type([
        id/0
      , task_id/0
      , speaker_id/0
      , engine_id/0
      , task/0
      , state/0
    ]).


%% package private
-export([
        start_link/0
      , init/1
      , handle_call/3
      , handle_cast/2
    ]).

-type id() :: klsn:binstr().

-type task_id() :: klsn:binstr().

-type speaker_id() :: non_neg_integer().

-type engine_id() :: non_neg_integer().

-type task() :: #{
        speaker_id := speaker_id()
      , payload := task_id()
    }.

-type state() :: #{
        id() => #{
            tasks := [#{
                speaker_id := speaker_id()
              , payload := task_id()
              , not_before := integer() %% millisecond
              , sleep := pos_integer() %% millisecond
            }]
          , loaded_tasks := #{
                task_id() => #{
                    speaker_id := speaker_id()
                  , payload := task_id()
                  , not_before := integer() %% millisecond
                  , sleep := pos_integer() %% millisecond
                  , done_at => integer() %% millisecond
                }
            }
          , overhead := pos_integer() %% millisecond
          , engine_count := pos_integer()
          , engine := #{
                engine_id() := #{
                    available_after := pos_integer() %% millisecond
                  , speaker_id => speaker_id()
                }
            }
        }
    }.

%% public functions

-spec new() -> id().
new() ->
    Id = klsn_binstr:uuid(),
    gen_server:cast(?MODULE, {new, Id}),
    Id.

-spec fetch_tasks(id()) -> [task()].
fetch_tasks(Id) ->
    {Tasks, SleepUntil} = call({fetch_tasks, Id}),
    sleep_until(SleepUntil),
    Tasks.

-spec set_engine_speaker(id(), engine_id(), speaker_id()) -> ok.
set_engine_speaker(Id, EId, SId) ->
    SleepUntil = call({speaker, Id, EId, SId}),
    sleep_until(SleepUntil),
    ok.

-spec synthesis(id(), engine_id(), speaker_id(), task_id()) -> ok.
synthesis(Id, EId, SId, TId) ->
    SleepUntil = call({synthesis, Id, EId, SId, TId}),
    sleep_until(SleepUntil),
    ok.


-spec config() -> #{}.
config() ->
    #{
        task_count => config(task_count)
      , engine_count => config(engine_count)
      , speaker_count => config(speaker_count)
    }.

-spec evaluate(id()) -> float().
evaluate(Id) ->
    LoadedTasks = call({loaded_tasks, Id}),
    Sum = maps:fold(fun
        (_, #{not_before := NBF, done_at := DoneAt}, Acc) ->
            Acc + DoneAt - NBF;
        (_, _, Acc) ->
            Acc + 1.0e9
    end, 0.0, LoadedTasks),
    Penalty = 1.0e9 * ( config(task_count) - maps:size(LoadedTasks) ),
    (Sum + Penalty) / config(task_count).
    

%% package private

config(task_count) ->
    application:get_env(vxmb, task_count, 100000);
config(overhead) ->
    application:get_env(vxmb, overhead, 3000);
config(engine_count) ->
    application:get_env(vxmb, engine_count, 4);
config(speaker_count) ->
    application:get_env(vxmb, speaker_count, 255);
config(sleep_min) ->
    application:get_env(vxmb, sleep_min, 100);
config(sleep_max) ->
    application:get_env(vxmb, sleep_max, 10000);
config(nbf_uniform) ->
    application:get_env(vxmb, nbf_uniform, 10000);
config(Arg1) ->
    erlang:error(badarg, Arg1).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Settings) ->
    State = #{
    },
    {ok, State}.

call(Arg1) ->
    case gen_server:call(?MODULE, Arg1) of
        {?MODULE, {raise, {Class, Reason, Stack}}} ->
            erlang:raise(Class, Reason, Stack);
        Res ->
            Res
    end.

handle_call(Arg1, Arg2, Arg3) ->
    try handle_call_(Arg1, Arg2, Arg3) catch
        Class:Reason:Stack ->
            {reply, {?MODULE, {raise, {Class, Reason, Stack}}}, Arg3}
    end.

handle_call_({fetch_tasks, Id}, _From, State0) when is_map_key(Id, State0) ->
    Tasks0 = klsn_map:get([Id, tasks], State0),
    LoadedTasks0 = klsn_map:get([Id, loaded_tasks], State0),
    {Ret, Tasks, NewLoadedTasks, SleepUntil} = fetch_tasks_(Tasks0),
    State10 = klsn_map:upsert([Id, tasks], Tasks, State0),
    LoadedTasks = maps:merge(LoadedTasks0, NewLoadedTasks),
    State = klsn_map:upsert([Id, loaded_tasks], LoadedTasks, State10),
    {reply, {Ret, SleepUntil}, State};
handle_call_({speaker, Id, EId, SId}, _, State0) when is_map_key(Id, State0) ->
    {BlockedUntil, Doc} = set_engine_speaker_(EId, SId, maps:get(Id, State0)),
    State = State0#{ Id => Doc },
    {reply, BlockedUntil, State};
handle_call_({synthesis, Id, EId, SId, TId}, _, State0) when is_map_key(Id, State0) ->
    {BlockedUntil, Doc} = synthesis_(EId, SId, TId, maps:get(Id, State0)),
    State = State0#{ Id => Doc },
    {reply, BlockedUntil, State};
handle_call_({loaded_tasks, Id}, _From, State) ->
    {reply, klsn_map:get([Id, loaded_tasks], State), State};
handle_call_(_, _From, State) ->
    {reply, invalid, State}.

handle_cast({new, Id}, State0) ->
    State = klsn_map:upsert([Id], new_(), State0),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.


fetch_tasks_([]) ->
    {[], [], #{}, time_now()};
fetch_tasks_(Tasks0) ->
    TimeNow = time_now(),
    LoadedTasks0 = lists:filtermap(fun
        (Task=#{not_before := NBF, payload := TaskId}) when NBF < TimeNow ->
            {true, {TaskId, Task}};
        (_) ->
            false
    end, Tasks0),
    {LoadedTasks, SleepUntil} = case LoadedTasks0 of
        [] ->
            [Task|_] = Tasks0,
            {[{maps:get(payload, Task), Task}], maps:get(not_before, Task)};
        _ ->
            {LoadedTasks0, TimeNow}
    end,
    {_, Tasks} = lists:split(length(LoadedTasks), Tasks0),
    Ret = lists:map(fun({_, Elem}) ->
        #{
            speaker_id => maps:get(speaker_id, Elem)
          , payload => maps:get(payload, Elem)
        }
    end, LoadedTasks),
    {Ret, Tasks, maps:from_list(LoadedTasks), SleepUntil}.

new_() ->
    TimeNow = time_now(),
    #{
        tasks => new_tasks_(config(task_count), [], TimeNow)
      , loaded_tasks => #{}
      , overhead => config(overhead)
      , engine_count => config(engine_count)
      , engine => maps:from_list(lists:zip(
            lists:seq(0, config(engine_count) - 1)
          , lists:duplicate(config(engine_count), #{available_after => TimeNow})
        ))
    }.

new_tasks_(0, Acc, _NBF) ->
    lists:reverse(Acc);
new_tasks_(N, Acc, NBF) ->
    new_tasks_(N-1, [#{
        speaker_id => rand:uniform(config(speaker_count)) - 1
      , payload => klsn_binstr:uuid()
      , not_before => NBF %% millisecond
      , sleep => config(sleep_min) + rand:uniform(config(sleep_max) - config(sleep_min)) - 1
    }|Acc], NBF + rand:uniform(config(nbf_uniform))).

set_engine_speaker_(EId, SId, Doc0) ->
    TimeNow = time_now(),
    BlockedUntil0 = klsn_map:get([engine, EId, available_after], Doc0),
    Overhead = case klsn_map:get([engine, EId], Doc0) of
        #{speaker_id := SId0} when SId0 =:= SId ->
            0;
        _ ->
            klsn_map:get([overhead], Doc0)
    end,
    BlockedUntil = case BlockedUntil0 > TimeNow of
        true ->
            BlockedUntil0 + Overhead;
        false ->
            TimeNow + Overhead
    end,
    Doc10 = klsn_map:upsert([engine, EId, speaker_id], SId, Doc0),
    Doc = klsn_map:upsert([engine, EId, available_after], BlockedUntil, Doc10),
    {BlockedUntil, Doc}.

synthesis_(EId, SId, TId, Doc0) ->
    SId = klsn_map:get([loaded_tasks, TId, speaker_id], Doc0),
    TimeNow = time_now(),
    BlockedUntil0 = klsn_map:get([engine, EId, available_after], Doc0),
    Overhead = klsn_map:get([loaded_tasks, TId, sleep], Doc0),
    BlockedUntil = case BlockedUntil0 > TimeNow of
        true ->
            BlockedUntil0 + Overhead;
        false ->
            TimeNow + Overhead
    end,
    Doc10 = klsn_map:upsert([loaded_tasks, TId, done_at], TimeNow, Doc0),
    Doc = klsn_map:upsert([engine, EId, available_after], BlockedUntil, Doc10),
    {BlockedUntil, Doc}.

time_now() ->
    erlang:system_time(millisecond).

sleep_until(SleepUntil) ->
    TimeNow = time_now(),
    case SleepUntil > TimeNow of
        true ->
            timer:sleep(SleepUntil - TimeNow);
        false ->
            ok
    end.
