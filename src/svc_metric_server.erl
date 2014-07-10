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
-export([start_link/7,
         start_link/9,
         stop/1]).

-export([get_status/1,
         get_values/1,
         get_histogram_statistics/1,
         update/2,
         resize/2,
         trim_and_notify/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEF_TIMEOUT, 30000).
-define(HOURSECS,    3600).

-record(metric, {tags = sets:new(),
                 type :: atom(),
                 history_size :: non_neg_integer()
                }).


%%--------------------------------------------------------------------
%% API-1
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(ServerId, SampleMod, ?METRIC_COUNTER = SampleType, Window, Callback, Step, ExpireTime) ->
    _ = folsom_metrics:new_counter(ServerId),
    gen_server:start_link({local, ServerId}, ?MODULE, [#sv_metric_state{id          = ServerId,
                                                                        sample_mod  = SampleMod,
                                                                        type        = SampleType,
                                                                        notify_to   = Callback,
                                                                        window      = Window,
                                                                        step        = Step,
                                                                        expire_time = ExpireTime
                                                                       }], []);

start_link(ServerId, SampleMod, ?METRIC_GAUGE = SampleType, Window, Callback, Step, ExpireTime) ->
    _ = folsom_metrics:new_gauge(ServerId),
    gen_server:start_link({local, ServerId}, ?MODULE, [#sv_metric_state{id          = ServerId,
                                                                        sample_mod  = SampleMod,
                                                                        type        = SampleType,
                                                                        notify_to   = Callback,
                                                                        window      = Window,
                                                                        step        = Step,
                                                                        expire_time = ExpireTime
                                                                       }], []).

start_link(ServerId, SampleMod, ?HISTOGRAM_SLIDE = SampleType,
           Window,_SampleSize,_Alpha, Callback, Step, ExpireTime) ->
    Sample = #slide{window = Window},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          notify_to   = Callback,
                                          window      = Window,
                                          step        = Step,
                                          expire_time = ExpireTime
                                         });
start_link(ServerId, SampleMod, ?HISTOGRAM_UNIFORM = SampleType,
           Window, SampleSize,_Alpha, Callback, Step, ExpireTime) ->
    Sample = #uniform{size = SampleSize},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          notify_to   = Callback,
                                          window      = Window,
                                          step        = Step,
                                          expire_time = ExpireTime
                                         });
start_link(ServerId, SampleMod, ?HISTOGRAM_EXDEC = SampleType,
           Window, SampleSize, Alpha, Callback, Step, ExpireTime) ->
    Now = folsom_utils:now_epoch(),
    Sample = #exdec{start = Now,
                    next  = Now + ?HOURSECS,
                    alpha = Alpha,
                    size  = SampleSize},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          notify_to   = Callback,
                                          window      = Window,
                                          step        = Step,
                                          expire_time = ExpireTime
                                         }).

%% @private
start_link_1(Sample, #sv_metric_state{id = ServerId} = State) ->
    ok = update_sample_conf(Sample, State),
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


trim_and_notify(ServerId) ->
    gen_server:cast(ServerId, trim_and_notify).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([State]) ->
    Now = leo_date:now(),
    ServerId = State#sv_metric_state.id,
    Window   = State#sv_metric_state.window,
    State_1  = State#sv_metric_state{updated_at = Now,
                                     trimed_at  = Now},
    timer:apply_after(timer:seconds(Window),
                      ?MODULE, trim_and_notify, [ServerId]),
    {ok, State_1}.


%% GET status
handle_call(get_status, _From, #sv_metric_state{id        = Id,
                                                type      = Type,
                                                window    = Window,
                                                notify_to = Callback} = State) ->
    Ret = [{'id', Id}, {'type', Type}, {'window', Window},
           {'notify_to', Callback}],

    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Ret}, State_1};


%% GET values - for counter
handle_call(get_values, _From, #sv_metric_state{id = ServerId,
                                                type = ?METRIC_COUNTER} = State) ->
    Count = folsom_metrics_counter:get_value(ServerId),
    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Count}, State_1};

%% GET values - for gauge
handle_call(get_values, _From, #sv_metric_state{id = ServerId,
                                                type = ?METRIC_GAUGE} = State) ->
    Gauge = folsom_metrics_gauge:get_value(ServerId),
    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Gauge}, State_1};

%% GET values - for histogram
handle_call(get_values, _From, #sv_metric_state{id = ServerId,
                                                sample_mod = Mod} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Ret = Mod:handle_to_get_values(Hist),

    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Ret}, State_1};

%% GET HISTO-STATS
handle_call(get_histogram_statistics, _From, #sv_metric_state{id = ServerId,
                                                              sample_mod = Mod} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Ret = Mod:handle_to_get_hist_stats(Hist),

    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Ret}, State_1};


%% Update a value - for counter
handle_call({update, Value}, _From, #sv_metric_state{id = ServerId,
                                                     type = ?METRIC_COUNTER} = State) ->
    folsom_metrics:notify({ServerId, {inc, Value}}),

    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, State_1};

%% Update a value - for gauge
handle_call({update, Value}, _From, #sv_metric_state{id = ServerId,
                                                     type = ?METRIC_GAUGE} = State) ->
    folsom_metrics:notify({ServerId, Value}),
    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, State_1};

%% Update a value - for histogram
handle_call({update, Value}, _From, #sv_metric_state{id = ServerId,
                                                     sample_mod = Mod} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Sample = Hist#histogram.sample,
    case Mod:handle_to_update(Hist#histogram.type, Hist#histogram.sample, Value) of
        Sample ->
            void;
        NewSample ->
            true = ets:insert(?HISTOGRAM_TABLE,
                              {ServerId, Hist#histogram{sample = NewSample}})
    end,

    State_1 = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, State_1}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast message
handle_cast({resize, NewSize}, #sv_metric_state{id = ServerId,
                                                type = SampleType} = State) ->
    [{_, Hist}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Sample_1 = Hist#histogram.sample,
    Sample_2 = case SampleType of
                   ?HISTOGRAM_UNIFORM ->
                       Sample_1#uniform{size = NewSize};
                   ?HISTOGRAM_EXDEC ->
                       Sample_1#exdec{size = NewSize};
                   _ ->
                       Sample_1
               end,
    case Sample_1 of
        Sample_2 ->
            void;
        _ ->
            ok = update_sample_conf(Sample_1, State)
    end,
    {noreply, State};

handle_cast(trim_and_notify, State) ->
    NewState = judge_trim_and_notify(State),
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, normal, State}.

%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


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
%% @doc Update a sample configuration
%% @private
update_sample_conf(Sample, #sv_metric_state{id   = ServerId,
                                            type = SampleType}) ->
    Hist = #histogram{type = SampleType, sample = Sample},
    true = ets:insert(?HISTOGRAM_TABLE, {ServerId, Hist}),
    true = ets:insert(?FOLSOM_TABLE, {ServerId, #metric{type = histogram}}),
    ok.


%% @doc Judge remove metrics or statistics
%%      and notify caluculated metrics/statistics to a client
%% @private
judge_trim_and_notify(#sv_metric_state{id = ServerId,
                                       expire_time = ExpireTime,
                                       window = Window,
                                       updated_at  = UpdatedAt} = State) ->
    %% Execute 'trim-and-notify' after the window's seconds
    timer:apply_after(timer:seconds(Window),
                      ?MODULE, trim_and_notify, [ServerId]),

    %% Remove oldest metrics or statistics
    %% and notify caluculated metrics/statistics to a client
    case ExpireTime of
        'infinity' ->
            trim_and_notify_1(State);
        _ ->
            Now = leo_date:now(),
            Diff = Now - UpdatedAt,
            case (Diff >= ExpireTime) of
                true ->
                    Delay = erlang:phash(Now, 500),
                    timer:apply_after(Delay, savanna_commons_sup,
                                      stop_child, [[ServerId]]),
                    State;
                false ->
                    trim_and_notify_1(State)
            end
    end.

%% @private
trim_and_notify_1(#sv_metric_state{sample_mod = Mod,
                                   window = Window,
                                   step = Step,
                                   trimed_at = TrimedAt} = State) ->
    Now  = leo_date:now(),
    Diff = Now - TrimedAt,

    case (Diff >= Window) of
        true ->
            Delay = erlang:phash(Now, 500),
            spawn(fun() ->
                          timer:sleep(erlang:phash2(leo_date:clock(), Delay)),
                          ToDateTime   = TrimedAt + Window,
                          AdjustedStep = adjust_step(ToDateTime, Window, Step),

                          catch Mod:trim_and_notify(
                                  State, #sv_result{from   = TrimedAt,
                                                    to     = ToDateTime,
                                                    window = Window,
                                                    adjusted_step = AdjustedStep
                                                   })
                  end),
            State#sv_metric_state{trimed_at = Now};
        false ->
            State
    end.

%% @private
adjust_step(DateTime, Window, Step) ->
    {D,{H,M,_}} = calendar:gregorian_seconds_to_datetime(
                    DateTime - leo_math:floor(Window/4)),
    AdjustedStep_1 = calendar:datetime_to_gregorian_seconds({D, {H,M,0}}),
    AdjustedStep_2 = case (Step > ?SV_STEP_1M)  of
                         true ->
                             StepMin = erlang:round(Step/60),
                             AdjustedStep_1 +
                                 (((M + (StepMin - (M rem StepMin))) - M) * 60);
                         false ->
                             AdjustedStep_1
                     end,
    AdjustedStep_2.


%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).
adjust_step_test_() ->
    {setup,
     fun () ->  ok end,
     fun (_) -> ok end,
     [
      {"test sliding counter-metrics",
       {timeout, 120, fun adjusted_step/0}}
     ]}.

adjusted_step() ->
    %% 1min
    _Ret_1 = adjust_step(leo_date:now(), ?SV_WINDOW_10S, ?SV_STEP_1M),
    _Ret_2 = adjust_step(leo_date:now(), ?SV_WINDOW_30S, ?SV_STEP_1M),
    _Ret_3 = adjust_step(leo_date:now(), ?SV_WINDOW_1M,  ?SV_STEP_1M),

    %% 5min
    _Ret_4 = adjust_step(leo_date:now(), ?SV_WINDOW_10S, ?SV_STEP_5M),
    _Ret_5 = adjust_step(leo_date:now(), ?SV_WINDOW_30S, ?SV_STEP_5M),
    _Ret_6 = adjust_step(leo_date:now(), ?SV_WINDOW_1M,  ?SV_STEP_5M),
    _Ret_7 = adjust_step(leo_date:now(), ?SV_WINDOW_5M,  ?SV_STEP_5M),

    {_,{_,M4,0}}= calendar:gregorian_seconds_to_datetime(_Ret_4),
    ?assertEqual(0, M4 rem  5),
    {_,{_,M5,0}}= calendar:gregorian_seconds_to_datetime(_Ret_4),
    ?assertEqual(0, M5 rem  5),
    {_,{_,M6,0}}= calendar:gregorian_seconds_to_datetime(_Ret_4),
    ?assertEqual(0, M6 rem  5),
    {_,{_,M7,0}}= calendar:gregorian_seconds_to_datetime(_Ret_4),
    ?assertEqual(0, M7 rem  5),

    %% 10min
    _Ret_8  = adjust_step(leo_date:now(), ?SV_WINDOW_10S, ?SV_STEP_10M),
    _Ret_9  = adjust_step(leo_date:now(), ?SV_WINDOW_30S, ?SV_STEP_10M),
    _Ret_10 = adjust_step(leo_date:now(), ?SV_WINDOW_1M,  ?SV_STEP_10M),
    _Ret_11 = adjust_step(leo_date:now(), ?SV_WINDOW_5M,  ?SV_STEP_10M),

    {_,{_,M8,0}} = calendar:gregorian_seconds_to_datetime(_Ret_8),
    ?assertEqual(0, M8  rem  10),
    {_,{_,M9,0}} = calendar:gregorian_seconds_to_datetime(_Ret_9),
    ?assertEqual(0, M9  rem  10),
    {_,{_,M10,0}}= calendar:gregorian_seconds_to_datetime(_Ret_10),
    ?assertEqual(0, M10 rem  10),
    {_,{_,M11,0}}= calendar:gregorian_seconds_to_datetime(_Ret_11),
    ?assertEqual(0, M11 rem  10),
    ok.

-endif.
