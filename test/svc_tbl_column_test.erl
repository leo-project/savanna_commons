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
-module(svc_tbl_column_test).
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
    Col_1 = #sv_column{id = {SchemaName, 'col_1'},
                       schema_name = SchemaName,
                       name = 'col_1',
                       type = ?COL_TYPE_COUNTER,
                       constraint = [{min, 0}, {max, 16384}],
                       created_at = leo_date:now()},
    Col_2 = #sv_column{id = {SchemaName, 'col_2'},
                       schema_name = SchemaName,
                       name = 'col_2',
                       type = ?COL_TYPE_H_SLIDE,
                       constraint = [],
                       created_at = leo_date:now()},
    Col_3 = #sv_column{id = {SchemaName, 'col_3'},
                       schema_name = SchemaName,
                       name = 'col_3',
                       type = ?COL_TYPE_H_UNIFORM,
                       constraint = [],
                       created_at = leo_date:now()},


    {atomic,ok} = svc_tbl_column:create_table(ram_copies, [node()]),
    not_found = svc_tbl_column:all(),
    not_found = svc_tbl_column:get(Col_1#sv_column.schema_name,
                                   Col_1#sv_column.name),

    ok = svc_tbl_column:update(Col_1),
    ok = svc_tbl_column:update(Col_2),
    ok = svc_tbl_column:update(Col_3),

    {ok, Ret_1} = svc_tbl_column:get(Col_1#sv_column.schema_name, Col_1#sv_column.name),
    {ok, Ret_2} = svc_tbl_column:get(Col_2#sv_column.schema_name, Col_2#sv_column.name),
    {ok, Ret_3} = svc_tbl_column:get(Col_3#sv_column.schema_name, Col_3#sv_column.name),
    ?debugVal(Ret_1),
    ?debugVal(Ret_2),
    ?debugVal(Ret_3),

    not_found = svc_tbl_column:get(SchemaName,'col_4'),
    Checksum_1 = svc_tbl_column:checksum(SchemaName),
    Size_1 = svc_tbl_column:size(),

    ?assertEqual(Col_1, Ret_1),
    ?assertEqual(Col_2, Ret_2),
    ?assertEqual(Col_3, Ret_3),
    ?assertEqual(3, Size_1),

    ok = svc_tbl_column:delete(Col_1#sv_column.schema_name, Col_1#sv_column.name),
    not_found = svc_tbl_column:get(Col_1#sv_column.schema_name, Col_1#sv_column.name),

    Checksum_2 = svc_tbl_column:checksum(SchemaName),
    ?assertEqual(true, Checksum_1 /= Checksum_2),

    Size_2 = svc_tbl_column:size(),
    ?assertEqual(2, Size_2),
    ok.
