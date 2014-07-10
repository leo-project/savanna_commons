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
             ok
     end,
     fun (_) ->
             ok
     end,
     [
      {"test transformer",
       {timeout, 30, fun transform/0}
      },
      {"test sync tables",
       {timeout, 30, fun sync/0}
      },
      {"test all functions",
       {timeout, 30, fun suite/0}
      }
     ]}.

transform() ->
    mnesia:start(),

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

    {atomic,ok} = mnesia:create_table(
                    ?TBL_COLUMNS,
                    [{ram_copies, [node()]},
                     {type, set},
                     {record_name, sv_column},
                     {attributes, record_info(fields, sv_column)},
                     {user_properties,
                      [{id,           tuple,       primary},
                       {schema_name,  atom,        false  },
                       {name,         atom,        false  },
                       {type,         atom,        false  },
                       {constraint,   list,        false  },
                       {created_at,   pos_integer, false  }
                      ]}
                    ]),
    lists:foreach(fun(Col) ->
                          F = fun()-> mnesia:write(?TBL_COLUMNS, Col, write) end,
                          leo_mnesia:write(F)
                  end, [Col_1,
                        Col_2,
                        Col_3]),
    ok = svc_tbl_column:transform(),
    {ok, Ret} = svc_tbl_column:all(),
    lists:foreach(fun(#?SV_COLUMN{}) ->
                          ok;
                     (_) ->
                          throw('bad_record')
                  end, Ret),

    mnesia:stop(),
    ok.


sync() ->
    mnesia:start(),

    SchemaName = 'test',
    Col_1 = #?SV_COLUMN{id = {SchemaName, 'col_1'},
                        schema_name = SchemaName,
                        name = 'col_1',
                        type = ?COL_TYPE_COUNTER,
                        constraint = [{min, 0}, {max, 16384}],
                        updated_at = leo_date:now(),
                        created_at = leo_date:now(),
                        del = false
                       },
    Col_2 = #?SV_COLUMN{id = {SchemaName, 'col_2'},
                        schema_name = SchemaName,
                        name = 'col_2',
                        type = ?COL_TYPE_H_SLIDE,
                        constraint = [],
                        updated_at = leo_date:now(),
                        created_at = leo_date:now(),
                        del = false
                       },
    Col_3 = #?SV_COLUMN{id = {SchemaName, 'col_3'},
                        schema_name = SchemaName,
                        name = 'col_3',
                        type = ?COL_TYPE_H_UNIFORM,
                        constraint = [],
                        updated_at = leo_date:now(),
                        created_at = leo_date:now(),
                        del = true
                       },
    Col_3_1 = #?SV_COLUMN{id = {SchemaName, 'col_3'},
                          schema_name = SchemaName,
                          name = 'col_3',
                          type = ?COL_TYPE_H_UNIFORM,
                          constraint = [],
                          updated_at = leo_date:now(),
                          created_at = leo_date:now(),
                          del = false
                         },
    Col_4 = #?SV_COLUMN{id = {SchemaName, 'col_4'},
                        schema_name = SchemaName,
                        name = 'col_4',
                        type = ?COL_TYPE_H_UNIFORM,
                        constraint = [],
                        updated_at = leo_date:now(),
                        created_at = leo_date:now(),
                        del = false
                       },

    {atomic,ok} = svc_tbl_column:create_table(ram_copies, [node()]),
    lists:foreach(fun(Col) ->
                          F = fun()-> mnesia:write(?TBL_COLUMNS, Col, write) end,
                          leo_mnesia:write(F)
                  end, [Col_1,
                        Col_2,
                        Col_3]),
    ok = svc_tbl_column:sync([Col_1,
                              Col_3_1,
                              Col_4]),

    {ok, Ret} = svc_tbl_column:all(),
    ?assertEqual(3, length(Ret)),

    lists:foreach(fun(Col) when Col == Col_1 ->
                          ok;
                     (Col) when Col == Col_3_1 ->
                          ok;
                     (Col) when Col == Col_4 ->
                          ok;
                     (_) ->
                          throw('bad_record')
                  end, Ret),

    mnesia:stop(),
    ok.


suite() ->
    mnesia:start(),

    SchemaName = 'test',
    Now =  leo_date:now(),
    Col_1 = #?SV_COLUMN{id = {SchemaName, 'col_1'},
                        schema_name = SchemaName,
                        name = 'col_1',
                        type = ?COL_TYPE_COUNTER,
                        constraint = [{min, 0}, {max, 16384}],
                        updated_at = Now,
                        created_at = Now},
    Col_2 = #?SV_COLUMN{id = {SchemaName, 'col_2'},
                        schema_name = SchemaName,
                        name = 'col_2',
                        type = ?COL_TYPE_H_SLIDE,
                        constraint = [],
                        updated_at = Now,
                        created_at = Now},
    Col_3 = #?SV_COLUMN{id = {SchemaName, 'col_3'},
                        schema_name = SchemaName,
                        name = 'col_3',
                        type = ?COL_TYPE_H_UNIFORM,
                        constraint = [],
                        updated_at = Now,
                        created_at = Now},
    Col_4 = #?SV_COLUMN{id = {SchemaName, 'col_4'},
                        schema_name = SchemaName,
                        name = 'col_4',
                        type = ?COL_TYPE_GAUGE,
                        constraint = [{min, 0}, {max, 16384}],
                        updated_at = Now,
                        created_at = Now},

    {atomic,ok} = svc_tbl_column:create_table(ram_copies, [node()]),
    not_found = svc_tbl_column:all(),
    not_found = svc_tbl_column:get(Col_1#?SV_COLUMN.schema_name,
                                   Col_1#?SV_COLUMN.name),

    ok = svc_tbl_column:insert(Col_1),
    ok = svc_tbl_column:insert(Col_2),
    ok = svc_tbl_column:insert(Col_3),
    ok = svc_tbl_column:insert(Col_4),

    {ok, Ret_1} = svc_tbl_column:get(Col_1#?SV_COLUMN.schema_name, Col_1#?SV_COLUMN.name),
    {ok, Ret_2} = svc_tbl_column:get(Col_2#?SV_COLUMN.schema_name, Col_2#?SV_COLUMN.name),
    {ok, Ret_3} = svc_tbl_column:get(Col_3#?SV_COLUMN.schema_name, Col_3#?SV_COLUMN.name),
    {ok, Ret_4} = svc_tbl_column:get(Col_4#?SV_COLUMN.schema_name, Col_4#?SV_COLUMN.name),

    not_found = svc_tbl_column:get(SchemaName,'col_5'),
    Checksum_1 = svc_tbl_column:checksum(SchemaName),
    Size_1 = svc_tbl_column:size(),

    ?assertEqual(Col_1, Ret_1),
    ?assertEqual(Col_2, Ret_2),
    ?assertEqual(Col_3, Ret_3),
    ?assertEqual(Col_4, Ret_4),
    ?assertEqual(4, Size_1),

    ok = svc_tbl_column:delete(Col_1#?SV_COLUMN.schema_name, Col_1#?SV_COLUMN.name),
    not_found = svc_tbl_column:get(Col_1#?SV_COLUMN.schema_name, Col_1#?SV_COLUMN.name),

    Checksum_2 = svc_tbl_column:checksum(SchemaName),
    ?assertEqual(true, Checksum_1 /= Checksum_2),

    Size_2 = svc_tbl_column:size(),
    ?assertEqual(4, Size_2),

    mnesia:stop(),
    ok.
