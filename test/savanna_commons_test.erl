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
-module(savanna_commons_test).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").


suite_test_() ->
    {setup,
     fun () ->
             mnesia:start(),
             {ok,_Pid} = savanna_commons_sup:start_link(),
             {atomic,ok} = svc_tbl_schema:create_table(ram_copies, [node()]),
             {atomic,ok} = svc_tbl_column:create_table(ram_copies, [node()]),
             {atomic,ok} = svc_tbl_metric_group:create_table(ram_copies, [node()]),
             ok
     end,
     fun (_) ->
             folsom:stop(),
             mnesia:stop()
     end,
     [
      {"test sliding counter-metrics",
       {timeout, 120, fun counter_metrics_1/0}},
      {"test sliding histogram_1",
       {timeout, 120, fun histogram_1/0}},
      {"test creating schema",
       {timeout, 120, fun create_schema/0}},
      {"test creating metrics by a schema",
       {timeout, 120, fun create_metrics_by_shcema/0}},
      {"test counter metrics for 60sec",
       {timeout, 120, fun counter_metrics_2/0}},
      {"test counter metrics for 60sec#1",
       {timeout, 120, fun histogram_2/0}},
      {"test counter metrics for 60sec#2",
       {timeout, 180, fun histogram_3/0}},
      {"test counter metrics for 60sec#3",
       {timeout, 180, fun histogram_4/0}}
     ]}.

counter_metrics_1() ->
    Schema = 'test',
    MetricGroup = 'test_grp_1',
    Key = 'c1',
    Window = 10,

    ok = savanna_commons:create_schema(
           Schema, [#sv_column{name = Key,
                               type = ?COL_TYPE_COUNTER,
                               constraint = []}
                   ]),
    ok = savanna_commons:create_metrics_by_schema(
           Schema, MetricGroup, Window, 'svc_nofify_sample'),

    savanna_commons:notify(MetricGroup, {Key, 128}),
    savanna_commons:notify(MetricGroup, {Key, 256}),
    savanna_commons:notify(MetricGroup, {Key, 384}),
    savanna_commons:notify(MetricGroup, {Key, 512}),

    {ok, Ret_1} = savanna_commons:get_metric_value(MetricGroup, Key),
    ?assertEqual(1280, Ret_1),

    timer:sleep(timer:seconds(Window + 3)),
    {ok, Ret_2} = savanna_commons:get_metric_value(MetricGroup, Key),
    ?assertEqual(0, Ret_2),

    %% After terminated procs, re-generate a metric and notify a msg to it
    timer:sleep(40000),

    savanna_commons:notify(MetricGroup, {Key, 128}),
    {ok, Ret_3} = svc_metric_server:get_values('sv_test_grp_1.c1'),
    ?assertEqual(128, Ret_3),

    mnesia:stop(),
    mnesia:start(),
    {atomic,ok} = svc_tbl_schema:create_table(ram_copies, [node()]),
    {atomic,ok} = svc_tbl_column:create_table(ram_copies, [node()]),
    {atomic,ok} = svc_tbl_metric_group:create_table(ram_copies, [node()]),
    ok.

histogram_1() ->
    Schema = 'test',
    MetricGroup = 'test_grp_2',
    Key = 'h1',
    Window = 10,

    ok = savanna_commons:create_schema(
           Schema, [
                    #sv_column{name = Key,
                               type = ?COL_TYPE_H_SLIDE,
                               constraint = []}
                   ]),
    ok = savanna_commons:create_metrics_by_schema(
           Schema, MetricGroup, Window, 'svc_nofify_sample'),

    savanna_commons:notify(MetricGroup, {Key,  16}),
    savanna_commons:notify(MetricGroup, {Key,  32}),
    savanna_commons:notify(MetricGroup, {Key,  64}),
    savanna_commons:notify(MetricGroup, {Key, 128}),
    savanna_commons:notify(MetricGroup, {Key, 128}),
    savanna_commons:notify(MetricGroup, {Key, 256}),
    savanna_commons:notify(MetricGroup, {Key, 512}),

    timer:sleep(1000),
    {ok, Ret} = savanna_commons:get_metric_value(MetricGroup, Key),
    ?assertEqual([16,32,64,128,128,256,512], Ret),

    {ok, Ret_1} = savanna_commons:get_histogram_statistics(MetricGroup, Key),
    ?assertEqual(16,  leo_misc:get_value('min',    Ret_1)),
    ?assertEqual(512, leo_misc:get_value('max',    Ret_1)),
    ?assertEqual(128, leo_misc:get_value('median', Ret_1)),
    ?assertEqual(7,   leo_misc:get_value('n',      Ret_1)),

    timer:sleep(timer:seconds(Window + 3)),
    {ok, Ret_2} = savanna_commons:get_histogram_statistics(MetricGroup, Key),
    ?assertEqual(0.0, leo_misc:get_value('min',    Ret_2)),
    ?assertEqual(0.0, leo_misc:get_value('max',    Ret_2)),
    ?assertEqual(0.0, leo_misc:get_value('median', Ret_2)),
    ?assertEqual(0,   leo_misc:get_value('n',      Ret_2)),
    ok.

create_schema() ->
    SchemaName = 'test_1',
    ok = savanna_commons:create_schema(
           SchemaName, [#sv_column{name = 'col_1',
                                   type = ?COL_TYPE_COUNTER,
                                   constraint = []},
                        #sv_column{name = 'col_2',
                                   type = ?COL_TYPE_H_UNIFORM,
                                   constraint = [{?HISTOGRAM_CONS_SAMPLE, 3000}]},
                        #sv_column{name = 'col_3',
                                   type = ?COL_TYPE_H_SLIDE,
                                   constraint = []},
                        #sv_column{name = 'col_4',
                                   type = ?COL_TYPE_H_EXDEC,
                                   constraint = [{?HISTOGRAM_CONS_SAMPLE, 3000},
                                                 {?HISTOGRAM_CONS_ALPHA,  0.018}]}
                       ]),

    {ok, Columns_1} = svc_tbl_column:all(),
    {ok, Columns_2} = svc_tbl_column:find_by_schema_name(SchemaName),
    ?assertEqual(5, length(Columns_1)),
    ?assertEqual(4, length(Columns_2)),
    ok.

create_metrics_by_shcema() ->
    Schema = 'test_1',
    MetricGroup = Schema,
    Window = 10,

    ok = savanna_commons:create_metrics_by_schema(
           Schema, MetricGroup, Window, 'svc_nofify_sample'),
    Key_1 = 'col_1',
    savanna_commons:notify(Schema, {Key_1, 128}),
    savanna_commons:notify(Schema, {Key_1, 256}),
    savanna_commons:notify(Schema, {Key_1, 384}),
    savanna_commons:notify(Schema, {Key_1, 512}),

    Key_2 = 'col_2',
    savanna_commons:notify(Schema, {Key_2,  16}),
    savanna_commons:notify(Schema, {Key_2,  32}),
    savanna_commons:notify(Schema, {Key_2,  64}),
    savanna_commons:notify(Schema, {Key_2, 128}),
    savanna_commons:notify(Schema, {Key_2, 256}),
    savanna_commons:notify(Schema, {Key_2, 384}),
    savanna_commons:notify(Schema, {Key_2, 512}),
    savanna_commons:notify(Schema, {Key_2, 1024}),

    Key_3 = 'col_3',
    Event_0 = erlang:phash2(leo_date:clock()),
    Event_1 = erlang:phash2(leo_date:clock()),
    Event_2 = erlang:phash2(leo_date:clock()),
    Event_3 = erlang:phash2(leo_date:clock()),
    Event_4 = erlang:phash2(leo_date:clock()),
    Event_5 = erlang:phash2(leo_date:clock()),
    Event_6 = erlang:phash2(leo_date:clock()),
    Event_7 = erlang:phash2(leo_date:clock()),
    Event_8 = erlang:phash2(leo_date:clock()),
    Event_9 = erlang:phash2(leo_date:clock()),

    savanna_commons:notify(Schema, {Key_3, Event_0}),
    savanna_commons:notify(Schema, {Key_3, Event_1}),
    savanna_commons:notify(Schema, {Key_3, Event_2}),
    savanna_commons:notify(Schema, {Key_3, Event_3}),
    savanna_commons:notify(Schema, {Key_3, Event_4}),
    savanna_commons:notify(Schema, {Key_3, Event_5}),
    savanna_commons:notify(Schema, {Key_3, Event_6}),
    savanna_commons:notify(Schema, {Key_3, Event_7}),
    savanna_commons:notify(Schema, {Key_3, Event_8}),
    savanna_commons:notify(Schema, {Key_3, Event_9}),

    Key_4 = 'col_4',
    savanna_commons:notify(Schema, {Key_4, Event_0}),
    savanna_commons:notify(Schema, {Key_4, Event_1}),
    savanna_commons:notify(Schema, {Key_4, Event_2}),
    savanna_commons:notify(Schema, {Key_4, Event_3}),
    savanna_commons:notify(Schema, {Key_4, Event_4}),
    savanna_commons:notify(Schema, {Key_4, Event_5}),
    savanna_commons:notify(Schema, {Key_4, Event_6}),
    savanna_commons:notify(Schema, {Key_4, Event_7}),
    savanna_commons:notify(Schema, {Key_4, Event_8}),
    savanna_commons:notify(Schema, {Key_4, Event_9}),

    timer:sleep(1000),
    {ok, Ret_1} = savanna_commons:get_metric_value(Schema, Key_1),
    {ok, Ret_2} = savanna_commons:get_histogram_statistics(Schema, Key_2),
    {ok, Ret_3} = savanna_commons:get_histogram_statistics(Schema, Key_3),
    {ok, Ret_4} = savanna_commons:get_histogram_statistics(Schema, Key_4),

    ?assertEqual(1280, Ret_1),
    ?assertEqual(true, [] /= Ret_2),
    ?assertEqual(true, [] /= Ret_3),
    ?assertEqual(true, [] /= Ret_3),
    ?assertEqual(true, [] /= Ret_4),

    timer:sleep(timer:seconds(Window + 3)),
    ok.

%% TEST metric counter
counter_metrics_2() ->
    Schema = 'test_counter',
    MetricGroup = Schema,
    Key = 'col_1',
    Window = 10,

    ok = savanna_commons:create_schema(
           Schema, [#sv_column{name = Key,
                               type = ?COL_TYPE_COUNTER,
                               constraint = []}
                   ]),
    ok = savanna_commons:create_metrics_by_schema(Schema, MetricGroup, Window, 'svc_nofify_sample'),

    StartTime = leo_date:now(),
    EndTime   = StartTime + 65,
    inspect_1(Schema, Key, StartTime, EndTime),
    ok.

inspect_1(_,_, CurrentTime, EndTime) when CurrentTime >= EndTime ->
    ok;
inspect_1(Schema, Key, _, EndTime) ->
    savanna_commons:notify(Schema, {Key, erlang:phash2(leo_date:clock(),127)}),
    inspect_1(Schema, Key, leo_date:now(), EndTime).


%% TEST metric histogram - (slide)
histogram_2() ->
    Schema = 'test_histogram_1',
    MetricGroup = Schema,
    Key = 'h1',
    Window = 10,

    ok = savanna_commons:create_schema(
           Schema, [
                    #sv_column{name = Key,
                               type = ?COL_TYPE_H_SLIDE,
                               constraint = []}
                   ]),
    ok = savanna_commons:create_metrics_by_schema(
           Schema, MetricGroup, Window, 'svc_nofify_sample'),


    StartTime = leo_date:now(),
    EndTime   = StartTime + 30,
    inspect_2(Schema, Key, StartTime, EndTime),
    ?debugVal("### DONE - histogram_2/0 ###"),
    ok.


%% TEST metric histogram - (uniform)
histogram_3() ->
    Schema = 'test_histogram_2',
    MetricGroup = Schema,
    Key = 'h1',
    SampleSize = 3000,
    Window = 10,

    ok = savanna_commons:create_schema(
           Schema, [
                    #sv_column{name = Key,
                               type = ?COL_TYPE_H_UNIFORM,
                               constraint = [{?HISTOGRAM_CONS_SAMPLE, SampleSize}]}
                   ]),
    ok = savanna_commons:create_metrics_by_schema(
           Schema, MetricGroup, Window, 'svc_nofify_sample'),


    StartTime = leo_date:now(),
    EndTime   = StartTime + 130,
    inspect_2(Schema, Key, StartTime, EndTime),
    ?debugVal("### DONE - histogram_3/0 ###"),
    ok.

%% TEST metric histogram - (exdec)
histogram_4() ->
    Schema = 'test_histogram_3',
    MetricGroup = Schema,
    Key = 'h1',
    SampleSize = 3000,
    Window = 10,

    ok = savanna_commons:create_schema(
           Schema, [
                    #sv_column{name = Key,
                               type = ?COL_TYPE_H_EXDEC,
                               constraint = [{?HISTOGRAM_CONS_SAMPLE, SampleSize}]}
                   ]),
    ok = savanna_commons:create_metrics_by_schema(
           Schema, MetricGroup, Window, 'svc_nofify_sample'),

    StartTime = leo_date:now(),
    EndTime   = StartTime + 130,
    inspect_2(Schema, Key, StartTime, EndTime),
    ?debugVal("### DONE - histogram_4/0 ###"),
    ok.


inspect_2(_,_, CurrentTime, EndTime) when CurrentTime >= EndTime ->
    ok;
inspect_2(Schema, Key, _, EndTime) ->
    savanna_commons:notify(Schema, {Key, erlang:phash2(leo_date:clock(), 1023)}),
    inspect_2(Schema, Key, leo_date:now(), EndTime).
