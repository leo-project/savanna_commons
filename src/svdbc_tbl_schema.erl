%%======================================================================
%%
%% LeoProject - SavannaDB
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
-module(svdbc_tbl_schema).
-author('Yosuke Hara').

-include("savannadb_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([create_table/2,
         all/0,
         get/1, update/1, delete/1,
         checksum/0, size/0
        ]).

-define(TBL_NAME, ?TBL_SCHEMAS).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create a table of system-configutation
%%
create_table(Mode, Nodes) ->
    mnesia:create_table(
      ?TBL_SCHEMAS,
      [{Mode, Nodes},
       {type, set},
       {record_name, svdb_schema},
       {attributes, record_info(fields, svdb_schema)},
       {user_properties,
        [{name,       atom,        primary},
         {created_at, pos_integer, false  }
        ]}
      ]).

%% @doc Retrieve all records
%%
-spec(all() ->
             {ok, [#svdb_schema{}]} | not_found | {error, any()}).
all() ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME)]),
                        Q2 = qlc:sort(Q1, [{order, descending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve a schema by name
%%
-spec(get(svdb_schema()) ->
             {ok, #svdb_schema{}} | not_found | {error, any()}).
get(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#svdb_schema.name == SchemaName]),
                        qlc:e(Q)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Modify a schema
%%
-spec(update(#svdb_schema{}) ->
             ok | {error, any()}).
update(Schema) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun()-> mnesia:write(?TBL_NAME, Schema, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Remove a schema
%%
-spec(delete(svdb_schema()) ->
             ok | {error, any()}).
delete(SchemaName) ->
    case ?MODULE:get(SchemaName) of
        {ok, Schema} ->
            Fun = fun() ->
                          mnesia:delete_object(?TBL_NAME, Schema, write)
                  end,
            leo_mnesia:delete(Fun);
        Error ->
            Error
    end.


%% @doc Retrieve the checksum of this table
%%
-spec(checksum() ->
             integer()).
checksum() ->
    case ?MODULE:all() of
        {ok, Records} ->
            erlang:phash2(Records);
        _ ->
            -1
    end.


%% @doc Retrieve the records
%%
-spec(size() ->
             pos_integer()).
size() ->
    mnesia:table_info(?TBL_NAME, size).
