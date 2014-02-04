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
-module(svdbc_tbl_schema_test).
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
    SchemaName_1 = 'test_1',
    SchemaName_2 = 'test_2',
    Schema_1 = #svdb_schema{name = SchemaName_1,
                            created_at = leo_date:now()},
    Schema_2 = #svdb_schema{name = SchemaName_2,
                            created_at = leo_date:now()},

    {atomic,ok} = svdbc_tbl_schema:create_table(ram_copies, [node()]),
    not_found = svdbc_tbl_schema:all(),
    not_found = svdbc_tbl_schema:get(SchemaName_1),
    not_found = svdbc_tbl_schema:get(SchemaName_2),

    ok = svdbc_tbl_schema:update(Schema_1),
    ok = svdbc_tbl_schema:update(Schema_2),
    Checksum_1 = svdbc_tbl_schema:checksum(),
    2 = svdbc_tbl_schema:size(),

    {ok, Schema_1} = svdbc_tbl_schema:get(SchemaName_1),
    {ok, Schema_2} = svdbc_tbl_schema:get(SchemaName_2),
    ok = svdbc_tbl_schema:delete(SchemaName_1),
    not_found = svdbc_tbl_schema:get(SchemaName_1),

    Checksum_2 = svdbc_tbl_schema:checksum(),
    ?assertEqual(true, Checksum_1 /= Checksum_2),

    1 = svdbc_tbl_schema:size(),
    ok.
