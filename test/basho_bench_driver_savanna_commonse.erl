%%======================================================================
%%
%% Savanna Commons
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
-module(basho_bench_driver_savanna_commonse).

-export([new/1,
         run/4,
         notify/2
        ]).

-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA, 'test').
-define(COL,    'col').

notify(_Schema, {_Key,_Values}) ->
        ok.


%% @doc initialize
%%
-spec(new(any()) ->
             ok).
new(1) ->
    %% prepare
    folsom:start(),
    mnesia:start(),
    {ok,_Pid} = savanna_commons_sup:start_link(),
    ok = svc_tbl_schema:create_table(ram_copies, [node()]),
    ok = svc_tbl_column:create_table(ram_copies, [node()]),
    ok = svc_tbl_metric_group:create_table(ram_copies, [node()]),

    %% create metrics
    ok = savanna_commons:create_schema(
           ?SCHEMA, [#?SV_COLUMN{name = ?COL,
                                type = 'counter',
                                constraint = []}
                    ]),

    %% Create a metric by the schema
    Window = 60,
    ok = savanna_commons:create_metrics_by_schema(?SCHEMA, Window, ?MODULE),
    {ok, null};
new(_) ->
    {ok, null}.


%% @doc run.
%%
-spec(run(get, any(), any(), any()) ->
             {ok, any()} | {error, any(), any()}).
run(put,_KeyGen, _ValueGen, State) ->
    savanna_commons:notify(?SCHEMA, {?COL, 128}),
    {ok, State}.

