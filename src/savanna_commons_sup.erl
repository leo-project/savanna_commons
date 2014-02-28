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
-module(savanna_commons_sup).
-author('Yosuke Hara').

-behaviour(supervisor).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         start_child/3, start_child/4, start_child/5,
         start_child/6, start_child/7,
         stop_slide_server/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(DEF_WINDOW, 30).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc Start children
%%
-spec(start_child('svc_metrics_counter', atom(), atom()) ->
             ok | {error, any()}).
start_child('svc_metrics_counter' = Mod, ServerId, Callback) ->
    start_child(Mod, ServerId, ?DEF_WINDOW, Callback).

-spec(start_child('svc_metrics_counter'|'svc_metrics_histogram', atom(),
                  pos_integer()|sv_histogram_type(), atom()) ->
             ok | {error, any()}).
start_child('svc_metrics_counter' = Mod, ServerId, Window, Callback) ->
    ProcExpirationTime = ?env_proc_expiration_time(),
    ChildSpec = {ServerId,
                 {svc_metric_server, start_link, [ServerId, Mod, ?METRIC_COUNTER,
                                                  Window, Callback, ProcExpirationTime]},
                 temporary, 2000, worker, [svc_metric_server]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_Pid} ->
            ok;
        {error, Cause} ->
            {error, Cause}
    end;

start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Callback) ->
    start_child(Mod, ServerId, HistogramType, ?DEF_WINDOW, Callback).

-spec(start_child('svc_metrics_histogram', atom(), sv_histogram_type(), pos_integer(), atom()) ->
             ok | {error, any()}).
start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Window, Callback) ->
    start_child(Mod, ServerId, HistogramType, Window, ?DEFAULT_SIZE, Callback).

-spec(start_child('svc_metrics_histogram', atom(), sv_histogram_type(), pos_integer(), pos_integer(), atom()) ->
             ok | {error, any()}).
start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Window, SampleSize, Callback) ->
    start_child(Mod, ServerId, HistogramType, Window, SampleSize, ?DEFAULT_ALPHA, Callback).

-spec(start_child('svc_metrics_histogram', atom(), sv_histogram_type(), pos_integer(), pos_integer(), float(), atom()) ->
             ok | {error, any()}).
start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Window,  SampleSize, Alpha, Callback) ->
    ProcExpirationTime = ?env_proc_expiration_time(),
    ChildSpec = {ServerId,
                 {svc_metric_server, start_link, [ServerId, Mod, HistogramType,
                                                  Window, SampleSize, Alpha, Callback, ProcExpirationTime]},
                 temporary, 2000, worker, [svc_metric_server]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_Pid} ->
            ok;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Stop a slide-server and a worker
%%
-spec(stop_slide_server(list(pid())) ->
             ok).
stop_slide_server(Pids) ->
    stop_slide_server_1(Pids).

%% @private
stop_slide_server_1([]) ->
    ok;
stop_slide_server_1([Pid|Rest]) ->
    catch supervisor:terminate_child(?MODULE, Pid),
    catch supervisor:delete_child(?MODULE, Pid),
    stop_slide_server_1(Rest).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Children = [
                {folsom,
                 {folsom_sup, start_link, []},
                 permanent, 2000, supervisor, [folsom]}
               ],
    {ok, {{one_for_one, 5, 60}, Children}}.
