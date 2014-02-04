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
-module(svdbc_tbl_column_test).
-author('Yosuke Hara').

-include("savannadb_commons.hrl").
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
    Col_1 = #svdb_column{id = 1,
                         schema_name = SchemaName,
                         name = 'col_1',
                         type = ?COL_TYPE_COUNTER,
                         constraint = [{min, 0}, {max, 16384}],
                         created_at = leo_date:now()},
    Col_2 = #svdb_column{id = 2,
                         schema_name = SchemaName,
                         name = 'col_2',
                         type = ?COL_TYPE_H_SLIDE,
                         constraint = [],
                         created_at = leo_date:now()},
    Col_3 = #svdb_column{id = 3,
                         schema_name = SchemaName,
                         name = 'col_3',
                         type = ?COL_TYPE_H_UNIFORM,
                         constraint = [],
                         created_at = leo_date:now()},


    {atomic,ok} = svdbc_tbl_column:create_table(ram_copies, [node()]),
    not_found = svdbc_tbl_column:all(),
    not_found = svdbc_tbl_column:get(Col_1#svdb_column.name),

    ok = svdbc_tbl_column:update(Col_1),
    ok = svdbc_tbl_column:update(Col_2),
    ok = svdbc_tbl_column:update(Col_3),

    {ok, Ret_1} = svdbc_tbl_column:get(Col_1#svdb_column.name),
    {ok, Ret_2} = svdbc_tbl_column:get(Col_2#svdb_column.name),
    {ok, Ret_3} = svdbc_tbl_column:get(Col_3#svdb_column.name),
    not_found = svdbc_tbl_column:get('col_4'),
    Checksum_1 = svdbc_tbl_column:checksum(SchemaName),

    ?assertEqual(Col_1, Ret_1),
    ?assertEqual(Col_2, Ret_2),
    ?assertEqual(Col_3, Ret_3),

    ok = svdbc_tbl_column:delete(Col_1#svdb_column.name),
    not_found = svdbc_tbl_column:get(Col_1#svdb_column.name),

    Checksum_2 = svdbc_tbl_column:checksum(SchemaName),
    ?assertEqual(true, Checksum_1 /= Checksum_2),
    ok.
