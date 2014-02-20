%%======================================================================
%%
%% LeoProject - Savanna Commons
%%
%% Copyright (c) 2014 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================
-module(svc_metric_server).

-behaviour(gen_server).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/5,
         start_link/7,
         stop/1]).

-export([get_status/1,
         get_values/1,
         get_histogram_statistics/1,
         update/2, resize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEF_WIDTH,   16).
-define(DEF_WINDOW,  60).
-define(DEF_TIMEOUT, 30000).
-define(HOURSECS,    3600).

-record(metric, {tags = sets:new() :: set(),
                 type :: atom(),
                 history_size :: pos_integer()
                }).


%%--------------------------------------------------------------------
%% API-1
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
-spec(start_link(atom(), atom(), ?METRIC_COUNTER, pos_integer(), atom()) ->
             {ok, #sv_metric_state{}} | {error, any()}).
start_link(ServerId, SampleMod, ?METRIC_COUNTER = SampleType, Window, Callback) ->
    _ = folsom_ets:add_handler(counter, ServerId),
    gen_server:start_link({local, ServerId}, ?MODULE, [#sv_metric_state{id          = ServerId,
                                                                        sample_mod  = SampleMod,
                                                                        type        = SampleType,
                                                                        window      = Window,
                                                                        notify_to   = Callback
                                                                       }], []).

-spec(start_link(atom(), atom(), sv_histogram_type(), pos_integer(), pos_integer(),
                 float(), function()) ->
             {ok, #sv_metric_state{}} | {error, any()}).
start_link(ServerId, SampleMod, ?HISTOGRAM_SLIDE = SampleType,
           Window,_SampleSize,_Alpha, Callback) ->
    Sample = #slide{window = Window},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          window      = Window,
                                          notify_to   = Callback,
                                          expire_time = ?SV_EXPIRE_TIME
                                         });
start_link(ServerId, SampleMod, ?HISTOGRAM_UNIFORM = SampleType,
           Window, SampleSize,_Alpha, Callback) ->
    Sample = #uniform{size = SampleSize},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          window      = Window,
                                          notify_to   = Callback,
                                          expire_time = ?SV_EXPIRE_TIME
                                         });
start_link(ServerId, SampleMod, ?HISTOGRAM_EXDEC = SampleType,
           Window, SampleSize, Alpha, Callback) ->
    Now = folsom_utils:now_epoch(),
    Sample = #exdec{start = Now,
                    next  = Now + ?HOURSECS,
                    alpha = Alpha,
                    size  = SampleSize},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          window      = Window,
                                          notify_to   = Callback,
                                          expire_time = ?SV_EXPIRE_TIME
                                         }).

%% @private
start_link_1(Sample, #sv_metric_state{id   = ServerId,
                                      type = SampleType} = State) ->
    Hist = #histogram{type = SampleType, sample = Sample},
    true = ets:insert(?HISTOGRAM_TABLE, {ServerId, Hist}),
    true = ets:insert(?FOLSOM_TABLE, {ServerId, #metric{type = histogram}}),
    gen_server:start_link({local, ServerId}, ?MODULE, [State], []).


stop(ServerId) ->
    gen_server:cast(ServerId, stop).


%%--------------------------------------------------------------------
%% API-2
%%--------------------------------------------------------------------
%% @doc Retrieve current status
-spec(get_status(atom()) ->
             {ok, list(tuple())}).
get_status(ServerId) ->
    gen_server:call(ServerId, get_status, ?DEF_TIMEOUT).


%% @doc Retrieve a metric
-spec(get_values(sv_metric()) ->
             ok | {error, any()}).
get_values(ServerId) ->
    gen_server:call(ServerId, get_values, ?DEF_TIMEOUT).


%% @doc Retrieve histogram-statistics
-spec(get_histogram_statistics(sv_metric()) ->
             ok | {error, any()}).
get_histogram_statistics(ServerId) ->
    gen_server:call(ServerId, get_histogram_statistics, ?DEF_TIMEOUT).


%% @doc Put a value
-spec(update(sv_metric(), any()) ->
             ok | {error, any()}).
update(ServerId, Value) ->
    gen_server:call(ServerId, {update, Value}, ?DEF_TIMEOUT).


%% @doc Resize the metric
-spec(resize(sv_metric(), pos_integer()) ->
             ok | {error, any()}).
resize(ServerId, NewSize) ->
    gen_server:cast(ServerId, {resize, NewSize}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([#sv_metric_state{window = Window} = State]) ->
    Now = leo_date:now(),
    {ok, State#sv_metric_state{updated_at = Now,
                               trimed_at  = Now}, Window}.


%% GET status
handle_call(get_status, _From, #sv_metric_state{id        = Id,
                                                type      = Type,
                                                window    = Window,
                                                notify_to = Callback} = State) ->
    Ret = [{'id', Id}, {'type', Type}, {'window', Window},
           {'notify_to', Callback}],

    State_1 = judge_trim_and_notify(State),
    State_2 = State_1#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Ret}, State_2, Window};


%% GET values
handle_call(get_values, _From, #sv_metric_state{id = ServerId,
                                                type = ?METRIC_COUNTER,
                                                window = Window} = State) ->
    Count = folsom_metrics_counter:get_value(ServerId),

    State_1 = judge_trim_and_notify(State),
    State_2 = State_1#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Count}, State_2, Window};

handle_call(get_values, _From, #sv_metric_state{id = ServerId,
                                                sample_mod = Mod,
                                                window = Window} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Ret = Mod:handle_get_values(Hist),

    State_1 = judge_trim_and_notify(State),
    State_2 = State_1#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Ret}, State_2, Window};

%% GET HISTO-STATS
handle_call(get_histogram_statistics, _From, #sv_metric_state{id = ServerId,
                                                              sample_mod = Mod,
                                                              window = Window} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Ret = Mod:handle_get_histogram_statistics(Hist),

    State_1 = judge_trim_and_notify(State),
    State_2 = State_1#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Ret}, State_2, Window};

%% Update a value
handle_call({update, Value}, _From, #sv_metric_state{id = ServerId,
                                                     type = ?METRIC_COUNTER,
                                                     window = Window} = State) ->
    folsom_metrics:notify({ServerId, {inc, Value}}),

    State_1 = judge_trim_and_notify(State),
    State_2 = State_1#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, State_2, Window};

handle_call({update, Value}, _From, #sv_metric_state{id = ServerId,
                                                     sample_mod = Mod,
                                                     window = Window} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Sample = Hist#histogram.sample,
    case Mod:handle_update(Hist#histogram.type, Hist#histogram.sample, Value) of
        Sample ->
            void;
        NewSample ->
            true = ets:insert(?HISTOGRAM_TABLE,
                              {ServerId, Hist#histogram{sample = NewSample}})
    end,

    State_1 = judge_trim_and_notify(State),
    State_2 = State_1#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, State_2, Window}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast message
handle_cast(stop, State) ->
    {stop, normal, State}.

%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info(timeout, #sv_metric_state{id = ServerId,
                                      window = Window,
                                      updated_at = UpdatedAt} = State) ->
    Now = leo_date:now(),
    case ?SV_EXPIRE_TIME of
        'infinity' ->
            trim_and_notify_1(State);
        ExpireTime ->
            Diff = Now - UpdatedAt,
            case (Diff >= ExpireTime) of
                true ->
                    timer:apply_after(
                      100, savanna_commons_sup, stop_slide_server, [[ServerId]]);
                false ->
                    trim_and_notify_1(State)
            end
    end,
    {noreply, State#sv_metric_state{trimed_at = Now}, Window};

handle_info(_Info, #sv_metric_state{window = Window} = State) ->
    {noreply, State, Window}.


%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.


%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @private
judge_trim_and_notify(#sv_metric_state{window = Window,
                                       trimed_at = TrimedAt} = State) ->
    Now  = leo_date:now(),
    Diff = Now - TrimedAt,

    case (Diff >= erlang:round(Window/1000)) of
        true ->
            ok = trim_and_notify_1(State),
            State#sv_metric_state{trimed_at = Now};
        false ->
            State
    end.


%% @private
trim_and_notify_1(#sv_metric_state{sample_mod = Mod} = State) ->
    spawn(fun() ->
                  timer:sleep(erlang:phash2(leo_date:clock(), 250)),
                  catch Mod:trim_and_notify(State)
          end),
    ok.
