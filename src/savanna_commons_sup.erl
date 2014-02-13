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

-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         start_slide_server/4,
         stop_slide_server/1
        ]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_slide_server(SampleMod, ServerId, Reservoir, Window) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [SampleMod, ServerId, Reservoir, Window]),
    Pid.

stop_slide_server(Pid) ->
    ?debugVal(Pid),
    catch supervisor:terminate_child(?MODULE, Pid),
    catch supervisor:delete_child(?MODULE, Pid),
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok,{{simple_one_for_one, 3, 180},
         [
          {undefined, {svc_sample_slide_server, start_link, []},
           temporary, brutal_kill, worker, [svc_sample_slide_server]}
         ]}}.
