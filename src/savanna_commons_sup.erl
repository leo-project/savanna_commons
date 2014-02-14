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
start_child('svc_metrics_counter' = Mod, ServerId, Callback) ->
    start_child(Mod, ServerId, ?DEF_WINDOW, Callback).
start_child('svc_metrics_counter' = Mod, ServerId, Window, Callback) ->
    case start_slide_server(Mod, ServerId, Window) of
        {ok, Server} ->
            ChildSpec = {ServerId,
                         {Mod, start_link, [ServerId, Window, Callback, Server]},
                         temporary, 2000, worker, [Mod]},

            case supervisor:start_child(?MODULE, ChildSpec) of
                {ok,_Pid} ->
                    ok;
                {error, Cause} ->
                    {error, Cause}
            end;
        {error, Cause} ->
            {error, Cause}
    end;

start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Callback) ->
    start_child(Mod, ServerId, HistogramType, ?DEF_WINDOW, Callback).
start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Window, Callback) ->
    start_child(Mod, ServerId, HistogramType, Window, ?DEFAULT_SIZE, Callback).
start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Window, SampleSize, Callback) ->
    start_child(Mod, ServerId, HistogramType, Window, SampleSize, ?DEFAULT_ALPHA, Callback).
start_child('svc_metrics_histogram' = Mod, ServerId, HistogramType, Window,  SampleSize, Alpha, Callback) ->
    case start_slide_server(Mod, ServerId, Window) of
        {ok, Server} ->
            ChildSpec = {ServerId,
                         {Mod, start_link, [ServerId, HistogramType, Window,
                                            SampleSize, Alpha, Callback, Server]},
                         temporary, 2000, worker, [Mod]},

            case supervisor:start_child(?MODULE, ChildSpec) of
                {ok,_Pid} ->
                    ok;
                {error, Cause} ->
                    {error, Cause}
            end;
        {error, Cause} ->
            {error, Cause}
    end.


%% @private
start_slide_server(SampleMod, ServerId, Window) ->
    Id = list_to_atom(lists:append([atom_to_list(ServerId),"_notifier"])),
    ChildSpec = {Id,
                 {svc_sample_slide_server, start_link, [SampleMod, ServerId, Window]},
                 temporary, 2000, worker, [svc_sample_slide_server]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_Pid} ->
            {ok, Id};
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
    {ok, {{one_for_one, 5, 60}, []}}.
