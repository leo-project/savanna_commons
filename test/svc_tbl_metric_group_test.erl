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
-module(svc_tbl_metric_group_test).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
    {setup,
     fun () ->
             mnesia:start()
     end,
     fun (_) ->
             mnesia:stop()
     end,
     [{"test all functions",
       {timeout, 30, fun suite/0}}
     ]}.

suite() ->
    SchemaName = 'test',
    MetricGrp_1 = #sv_metric_group{id = {SchemaName, 'metic_group_1'},
                                   schema_name = SchemaName,
                                   name = 'metic_group_1',
                                   window = 5000,
                                   callback = 'svc_nofify_sample',
                                   created_at = leo_date:now()},
    MetricGrp_2 = #sv_metric_group{id = {SchemaName, 'metic_group_2'},
                                   schema_name = SchemaName,
                                   name = 'metic_group_2',
                                   window = 5000,
                                   callback = 'svc_nofify_sample',
                                   created_at = leo_date:now()},
    MetricGrp_3 = #sv_metric_group{id = {SchemaName, 'metic_group_3'},
                                   schema_name = SchemaName,
                                   name = 'metic_group_3',
                                   window = 5000,
                                   callback = 'svc_nofify_sample',
                                   created_at = leo_date:now()},


    {atomic,ok} = svc_tbl_metric_group:create_table(ram_copies, [node()]),
    not_found = svc_tbl_metric_group:all(),
    not_found = svc_tbl_metric_group:get(MetricGrp_1#sv_metric_group.name),

    ok = svc_tbl_metric_group:update(MetricGrp_1),
    ok = svc_tbl_metric_group:update(MetricGrp_2),
    ok = svc_tbl_metric_group:update(MetricGrp_3),

    {ok, Ret_1} = svc_tbl_metric_group:get(MetricGrp_1#sv_metric_group.name),
    {ok, Ret_2} = svc_tbl_metric_group:get(MetricGrp_2#sv_metric_group.name),
    {ok, Ret_3} = svc_tbl_metric_group:get(MetricGrp_3#sv_metric_group.name),

    not_found = svc_tbl_metric_group:get('metric_group_4'),
    Checksum_1 = svc_tbl_metric_group:checksum(SchemaName),
    Size_1 = svc_tbl_metric_group:size(),

    ?assertEqual(MetricGrp_1, Ret_1),
    ?assertEqual(MetricGrp_2, Ret_2),
    ?assertEqual(MetricGrp_3, Ret_3),
    ?assertEqual(3, Size_1),

    ok = svc_tbl_metric_group:delete(MetricGrp_1#sv_metric_group.name),
    not_found = svc_tbl_metric_group:get(MetricGrp_1#sv_metric_group.name),

    Checksum_2 = svc_tbl_metric_group:checksum(SchemaName),
    ?assertEqual(true, Checksum_1 /= Checksum_2),

    Size_2 = svc_tbl_metric_group:size(),
    ?assertEqual(2, Size_2),
    ok.
