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
-module(svc_tbl_schema_test).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA_NAME_1, << "test_1" >>).
-define(SCHEMA_NAME_2, << "test_2" >>).
-define(SCHEMA_NAME_3, << "test_3" >>).
-define(SCHEMA_1, #?SV_SCHEMA{name = ?SCHEMA_NAME_1,
                              name_string = binary_to_list(?SCHEMA_NAME_1),
                              created_at = leo_date:now()}).
-define(SCHEMA_2, #?SV_SCHEMA{name = ?SCHEMA_NAME_2,
                              name_string = binary_to_list(?SCHEMA_NAME_2),
                              created_at = leo_date:now()}).
-define(SCHEMA_3, #?SV_SCHEMA{name = ?SCHEMA_NAME_3,
                              name_string = binary_to_list(?SCHEMA_NAME_3),
                              created_at = leo_date:now()}).

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
      {"test sync table",
       {timeout, 30, fun sync/0}
      },
      {"test all functions",
       {timeout, 30, fun suite/0}
      }
     ]}.

transform() ->
    mnesia:start(),

    %% -record(sv_schema, {
    %%           name           :: sv_schema(),
    %%           created_at = 0 :: integer()
    %%          }).
    %% -record(sv_schema_1, {
    %%           name             :: sv_schema(),
    %%           name_string = [] :: string(),
    %%           created_at = 0   :: integer()
    %%          }).

    Schema_1 = #sv_schema{name = <<"S1">>,
                          created_at = leo_date:now()},
    Schema_2 = #sv_schema{name = <<"S2">>,
                          created_at = leo_date:now()},
    Schema_3 = #sv_schema{name = <<"S3">>,
                          created_at = leo_date:now()},

    ok = case mnesia:create_table(
                ?TBL_SCHEMAS,
                [{ram_copies, [node()]},
                 {type, set},
                 {record_name, sv_schema},
                 {attributes, record_info(fields, sv_schema)},
                 {user_properties,
                  [{name,        binary,      primary},
                   {created_at,  pos_integer, false  }
                  ]}
                ]) of
             {atomic, ok} ->
                 ok;
             {aborted,{already_exists,_}} ->
                 ok;
             {aborted,Error} ->
                 {error, Error}
         end,

    lists:foreach(fun(S) ->
                          F = fun()-> mnesia:write(?TBL_SCHEMAS, S, write) end,
                          ok = leo_mnesia:write(F),
                          ok
                  end, [Schema_1,
                        Schema_2,
                        Schema_3]),

    ok = svc_tbl_schema:transform(),
    {ok, Ret} = svc_tbl_schema:all(),
    lists:foreach(fun(#?SV_SCHEMA{}) ->
                          ok;
                     (_) ->
                          throw('bad_record')
                  end, Ret),

    mnesia:stop(),
    ok.

sync() ->
    mnesia:start(),

    ok = svc_tbl_schema:create_table(ram_copies, [node()]),
    Schema_1 = ?SCHEMA_1,
    Schema_2 = ?SCHEMA_2,
    Schema_3 = ?SCHEMA_3,

    ok = svc_tbl_schema:insert(Schema_1),
    ok = svc_tbl_schema:insert(Schema_2),

    ok = svc_tbl_schema:sync([Schema_1,
                              Schema_3]),
    {ok, Ret} = svc_tbl_schema:all(),
    ?assertEqual(2, length(Ret)),

    lists:foreach(fun(S) when S == Schema_1 ->
                          ok;
                     (S) when S == Schema_3 ->
                          ok;
                     (_) ->
                          throw('bad_record')
                  end, Ret),

    mnesia:stop(),
    ok.


suite() ->
    mnesia:start(),

    ok = svc_tbl_schema:create_table(ram_copies, [node()]),
    not_found = svc_tbl_schema:all(),
    not_found = svc_tbl_schema:get(?SCHEMA_1),
    not_found = svc_tbl_schema:get(?SCHEMA_2),

    ok = svc_tbl_schema:insert(?SCHEMA_1),
    ok = svc_tbl_schema:insert(?SCHEMA_2),
    Checksum_1 = svc_tbl_schema:checksum(),
    2 = svc_tbl_schema:size(),

    {ok, _S1} = svc_tbl_schema:get(?SCHEMA_NAME_1),
    {ok, _S2} = svc_tbl_schema:get(?SCHEMA_NAME_2),

    ?assertEqual(?SCHEMA_1, _S1),
    ?assertEqual(?SCHEMA_2, _S2),

    {ok, _S1} = svc_tbl_schema:find_by_name_string(binary_to_list(?SCHEMA_NAME_1)),
    {ok, _S2} = svc_tbl_schema:find_by_name_string(binary_to_list(?SCHEMA_NAME_2)),

    ok = svc_tbl_schema:delete(?SCHEMA_NAME_1),
    not_found = svc_tbl_schema:get(?SCHEMA_NAME_1),

    Checksum_2 = svc_tbl_schema:checksum(),
    ?assertEqual(true, Checksum_1 /= Checksum_2),

    1 = svc_tbl_schema:size(),

    mnesia:stop(),
    ok.
