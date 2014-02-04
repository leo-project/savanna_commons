%%======================================================================
%%
%% LeoProject - SavannaDB Commons
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
-module(savannadb_commons_test).
-author('Yosuke Hara').

-include("savannadb_commons.hrl").
-include_lib("eunit/include/eunit.hrl").


suite_test_() ->
    {setup,
     fun () ->
             folsom:start(),
             {ok,_Pid} = svdbc_sup:start_link(),
             ok
     end,
     fun (_) ->
             folsom:stop()
     end,
     [{"test sliding counter-metrics",
       {timeout, 30, fun counter_metrics/0}},
      {"resize sliding histogram",
       {timeout, 30, fun histogram/0}}
     ]}.

counter_metrics() ->
    Schema = 'test',
    Key = 'c1',
    Window = 10,
    Callback = fun(_Value) ->
                       ?debugVal(_Value),
                       ok
               end,
    savannadb_commons:new(?METRIC_COUNTER, Schema, Key, Window, Callback),
    savannadb_commons:notify(Schema, {Key, 128}),
    savannadb_commons:notify(Schema, {Key, 256}),
    savannadb_commons:notify(Schema, {Key, 384}),
    savannadb_commons:notify(Schema, {Key, 512}),
    {ok, Ret_1} = savannadb_commons:get_metric_value(Schema, Key),
    ?assertEqual([{count,1280},{one,1280}], Ret_1),

    %% @TODO - check sent value into the db
    timer:sleep(Window * 2000 + 100),
    {ok, Ret_2} = savannadb_commons:get_metric_value(Schema, Key),
    ?assertEqual([{count,1280},{one,0}], Ret_2),
    ok.

histogram() ->
    Schema = 'test',
    Key = 'h1',
    Window = 10,
    Callback = fun(_Value) ->
                       ?debugVal(_Value),
                       ok
               end,
    savannadb_commons:new(?METRIC_HISTOGRAM, ?HISTOGRAM_SLIDE, Schema, Key, Window, Callback),
    savannadb_commons:notify(Schema, {Key,  16}),
    savannadb_commons:notify(Schema, {Key,  32}),
    savannadb_commons:notify(Schema, {Key,  64}),
    savannadb_commons:notify(Schema, {Key, 128}),
    savannadb_commons:notify(Schema, {Key, 128}),
    savannadb_commons:notify(Schema, {Key, 256}),
    savannadb_commons:notify(Schema, {Key, 512}),

    {ok, Ret} = savannadb_commons:get_metric_value(Schema, Key),
    ?assertEqual([16,32,64,128,128,256,512], Ret),

    {ok, Ret_1} = savannadb_commons:get_histogram_statistics(Schema, Key),
    ?assertEqual(16,  leo_misc:get_value('min',    Ret_1)),
    ?assertEqual(512, leo_misc:get_value('max',    Ret_1)),
    ?assertEqual(128, leo_misc:get_value('median', Ret_1)),
    ?assertEqual(7,   leo_misc:get_value('n',      Ret_1)),

    %% @TODO - check sent value into the db
    timer:sleep(Window * 2000 + 100),
    {ok, Ret_2} = savannadb_commons:get_histogram_statistics(Schema, Key),
    ?assertEqual(0.0, leo_misc:get_value('min',    Ret_2)),
    ?assertEqual(0.0, leo_misc:get_value('max',    Ret_2)),
    ?assertEqual(0.0, leo_misc:get_value('median', Ret_2)),
    ?assertEqual(0,   leo_misc:get_value('n',      Ret_2)),
    ok.
