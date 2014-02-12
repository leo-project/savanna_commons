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
-module(svc_metrics_histogram).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/3, start_link/4, start_link/5, start_link/6,
         stop/1]).

-export([update/2,
         get_values/1,
         get_histogram_statistics/1,
         resize/2,
         trim/3
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name :: atom(),
                sample_type :: sv_histogram_type(),
                window = 0  :: pos_integer(),
                reservoir   :: pos_integer(),
                server      :: pid(),
                callback    :: atom() %% see:'svc_notify_behaviour'
               }).

-record(metric, {tags = sets:new() :: set(),
                 type :: atom(),
                 history_size :: pos_integer()
                }).

-define(DEF_WIDTH,   16).
-define(DEF_WINDOW,  60).
-define(DEF_TIMEOUT, 30000).
-define(HOURSECS,    3600).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
-spec(start_link(atom(), sv_histogram_type(), function()) ->
             {ok, pid()} | {error, any()}).
start_link(Name, HistogramType, Callback) ->
    start_link(Name, HistogramType, ?DEF_WINDOW, Callback()).


-spec(start_link(atom(), sv_histogram_type(), pos_integer(), function()) ->
             {ok, pid()} | {error, any()}).
start_link(Name, HistogramType, Window, Callback) ->
    start_link(Name, HistogramType, Window, ?DEFAULT_SIZE, Callback).

-spec(start_link(atom(), sv_histogram_type(), pos_integer(), pos_integer(), function()) ->
             {ok, pid()} | {error, any()}).
start_link(Name, ?HISTOGRAM_SLIDE_UNIFORM = HistogramType, Window, SampleSize, Callback) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, HistogramType, Window,
                           {Window, SampleSize}, ?DEFAULT_ALPHA, Callback], []);

start_link(Name, HistogramType, Window, SampleSize, Callback) ->
    start_link(Name, HistogramType, Window, SampleSize, ?DEFAULT_ALPHA, Callback).

-spec(start_link(atom(), sv_histogram_type(), pos_integer(), pos_integer(), float(), function()) ->
             {ok, pid()} | {error, any()}).
start_link(Name, HistogramType, Window, SampleSize, Alpha, Callback) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, HistogramType, Window, SampleSize, Alpha, Callback], []).

stop(Name) ->
    gen_server:call(Name, stop).


%% @doc Retrieve value
-spec(get_values(sv_metric()) ->
             {ok, list()}).
get_values(Name) ->
    gen_server:call(Name, get_values, ?DEF_TIMEOUT).


%% @doc Retrieve histogram-stat
-spec(get_histogram_statistics(sv_metric()) ->
             {ok, list()} | not_found | {error, any()}).
get_histogram_statistics(Name) ->
    gen_server:call(Name, get_histogram_statistics, ?DEF_TIMEOUT).


%% @doc Put a value
-spec(update(sv_metric(), any()) ->
             ok | {error, any()}).
update(Name, Value) ->
    gen_server:call(Name, {update, Value}, ?DEF_TIMEOUT).


%% @doc Resize the metric
-spec(resize(sv_metric(), pos_integer()) ->
             ok | {error, any()}).
resize(Name, NewSize) ->
    gen_server:call(Name, {resize, NewSize}, ?DEF_TIMEOUT).


%% @doc Remove values from the stats
-spec(trim(atom(), atom(), pos_integer()) ->
             ok | {error, any()}).
trim(Name, Tid, Window) ->
    gen_server:call(Name, {trim, Tid, Window}, ?DEF_TIMEOUT).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server

init([Name, ?HISTOGRAM_SLIDE = SampleType, Window,_SampleSize,_Alpha, Callback]) ->
    Sample = #slide{window = Window},
    Reservoir = Sample#slide.reservoir,
    init_1(Sample, #state{name = Name,
                          sample_type = SampleType,
                          window = Window,
                          reservoir = Reservoir,
                          callback  = Callback
                         });

init([Name, ?HISTOGRAM_SLIDE_UNIFORM = SampleType, Window, SampleSize,_Alpha, Callback]) ->
    Sample = #slide_uniform{window = Window, size = SampleSize},
    Reservoir = Sample#slide_uniform.reservoir,
    init_1(Sample, #state{name = Name,
                          sample_type = SampleType,
                          window = Window,
                          reservoir = Reservoir,
                          callback  = Callback
                         });
init([Name, ?HISTOGRAM_UNIFORM = SampleType, Window, SampleSize,_Alpha, Callback]) ->
    Sample = #uniform{size = SampleSize},
    Reservoir = Sample#uniform.reservoir,
    init_1(Sample, #state{name = Name,
                          sample_type = SampleType,
                          window = Window,
                          reservoir = Reservoir,
                          callback  = Callback
                         });
init([Name, ?HISTOGRAM_EXDEC = SampleType, Window, SampleSize, Alpha, Callback]) ->
    Now = folsom_utils:now_epoch(),
    Sample = #exdec{start = Now,
                    next  = Now + ?HOURSECS,
                    alpha = Alpha,
                    size  = SampleSize},
    Reservoir = Sample#exdec.reservoir,
    init_1(Sample, #state{name = Name,
                          sample_type = SampleType,
                          window = Window,
                          reservoir = Reservoir,
                          callback  = Callback
                         }).
%% @private
init_1(Sample, #state{name = Name,
                      sample_type = SampleType,
                      window = Window,
                      reservoir = Reservoir} = State) ->
    Pid = savanna_commons_sup:start_slide_server(?MODULE, Name, Reservoir, Window),
    Hist = #histogram{type = SampleType, sample = Sample},
    true = ets:insert(?HISTOGRAM_TABLE, {Name, Hist}),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = histogram}}),
    {ok, State#state{server = Pid}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_values, _From, #state{name = Name,
                                      sample_type = SampleType} = State) ->
    Hist = get_value(Name),
    Reply = get_values_1(SampleType, Hist#histogram.sample),
    {reply, Reply, State};

handle_call(get_histogram_statistics, _From, #state{name = Name,
                                                    sample_type = SampleType} = State) ->
    CurrentStat = get_current_statistics(Name, SampleType),
    {reply, {ok, CurrentStat}, State};

handle_call({update, Value}, _From, #state{name = Name} = State) ->
    Hist = get_value(Name),
    Sample = Hist#histogram.sample,
    case update_1(Hist#histogram.type, Hist#histogram.sample, Value) of
        Sample ->
            void;
        NewSample ->
            true = ets:insert(?HISTOGRAM_TABLE,
                              {Name, Hist#histogram{sample = NewSample}})
    end,
    {reply, ok, State};

handle_call({resize, NewSize}, _From, #state{server = Pid} = State) ->
    ok = svc_sample_slide_server:resize(Pid, NewSize),
    {reply, ok, State#state{window = NewSize}};

handle_call({trim, Reservoir, Window}, _From, #state{name = Name,
                                                     sample_type = SampleType,
                                                     callback = Callback} = State) ->
    %% Retrieve the current value, then execute the callback-function
    case is_atom(Callback) of
        true ->
            {SchemaName, Key} = ?sv_schema_and_key(Name),
            CurrentStat = get_current_statistics(Name, SampleType),
            catch Callback:notify(SchemaName, {Key, CurrentStat});
        false ->
            void
    end,

    %% Remove oldest data
    trim_1(SampleType, Reservoir, Window),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Retrieve the histogram record from ets
%% @private
get_value(Name) ->
    [{_, Value}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    Value.

%% @doc Retrieve values
%% @private
get_values_1(?HISTOGRAM_SLIDE, Sample) ->
    folsom_sample_slide:get_values(Sample);
get_values_1(?HISTOGRAM_SLIDE_UNIFORM, Sample) ->
    folsom_sample_slide_uniform:get_values(Sample);
get_values_1(?HISTOGRAM_UNIFORM, Sample) ->
    folsom_sample_uniform:get_values(Sample);
get_values_1(?HISTOGRAM_EXDEC, Sample) ->
    folsom_sample_exdec:get_values(Sample).


%% @doc Retrieve the current statistics
%% @private
get_current_statistics(Name, SampleType) ->
    Hist = get_value(Name),
    Values = get_values_1(SampleType, Hist#histogram.sample),
    bear:get_statistics(Values).


%% @doc Insert the value
%% @private
update_1(?HISTOGRAM_SLIDE, Sample, Value) ->
    folsom_sample_slide:update(Sample, Value);
update_1(?HISTOGRAM_SLIDE_UNIFORM, Sample, Value) ->
    folsom_sample_slide_uniform:update(Sample, Value);
update_1(?HISTOGRAM_UNIFORM, Sample, Value) ->
    folsom_sample_uniform:update(Sample, Value);
update_1(?HISTOGRAM_EXDEC, Sample, Value) ->
    folsom_sample_exdec:update(Sample, Value).

%% @doc Remove oldest values
%% @private
trim_1(?HISTOGRAM_SLIDE, Reservoir, Window) ->
    folsom_sample_slide:trim(Reservoir, Window);
trim_1(?HISTOGRAM_SLIDE_UNIFORM, Reservoir, Window) ->
    folsom_sample_slide_uniform:trim(Reservoir, Window);
trim_1(?HISTOGRAM_UNIFORM,_,_) ->
    ok;
trim_1(?HISTOGRAM_EXDEC,_,_) ->
    ok.
