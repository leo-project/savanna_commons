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
         update/2, update/5,
         resize/2]).

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
                                                                        notify_to   = Callback,
                                                                        expire_time = ?SV_EXPIRE_TIME,
                                                                        updated_at  = leo_date:now()
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
                                          expire_time = ?SV_EXPIRE_TIME,
                                          updated_at  = leo_date:now()
                                         });
start_link(ServerId, SampleMod, ?HISTOGRAM_UNIFORM = SampleType,
           Window, SampleSize,_Alpha, Callback) ->
    Sample = #uniform{size = SampleSize},
    start_link_1(Sample, #sv_metric_state{id          = ServerId,
                                          sample_mod  = SampleMod,
                                          type        = SampleType,
                                          window      = Window,
                                          notify_to   = Callback,
                                          expire_time = ?SV_EXPIRE_TIME,
                                          updated_at  = leo_date:now()
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
                                          expire_time = ?SV_EXPIRE_TIME,
                                          updated_at  = leo_date:now()
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


%% @doc Put a value
-spec(update(sv_metric(), any()) ->
             ok | {error, any()}).
update(ServerId, Value) ->
    gen_server:call(ServerId, {update, Value}, ?DEF_TIMEOUT).

update(ServerId, Value, Histogram, Sample, Callback) ->
    gen_server:call(ServerId, {update, Value, Histogram,
                               Sample, Callback}, ?DEF_TIMEOUT).


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
    {ok, State, Window}.


handle_call(get_status, _From, #sv_metric_state{id        = Id,
                                                type      = Type,
                                                window    = Window,
                                                notify_to = Callback} = State) ->
    Reply = [{'id', Id}, {'type', Type}, {'window', Window},
             {'notify_to', Callback}],
    NewState = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, {ok, Reply}, NewState, Window};

handle_call({update, Value}, _From, #sv_metric_state{id = ServerId,
                                                     type = ?METRIC_COUNTER,
                                                     window = Window} = State) ->
    folsom_metrics:notify({ServerId, {inc, Value}}),
    NewState = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, NewState, Window};

handle_call({update, Value, Hist, Sample, Callback}, _From, #sv_metric_state{id = ServerId,
                                                                             window = Window} = State) ->
    case Callback(Hist#histogram.type, Hist#histogram.sample, Value) of
        Sample ->
            void;
        NewSample ->
            true = ets:insert(?HISTOGRAM_TABLE,
                              {ServerId, Hist#histogram{sample = NewSample}})
    end,
    NewState = State#sv_metric_state{updated_at = leo_date:now()},
    {reply, ok, NewState, Window}.


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
                                      sample_mod = SampleMod,
                                      window = Window,
                                      updated_at = UpdatedAt} = State) ->
    Diff = leo_date:now() - UpdatedAt,
    case (Diff >= ?SV_EXPIRE_TIME) of
        true ->
            timer:apply_after(
              100, savanna_commons_sup, stop_slide_server, [[ServerId]]);
        false ->
            timer:sleep(erlang:phash2(leo_date:clock(), 250)),
            catch SampleMod:trim_and_notify(State)
    end,
    {noreply, State, Window};

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
