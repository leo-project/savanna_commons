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
-module(svdbc_tbl_column).
-author('Yosuke Hara').

-include("savannadb_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([create_table/2,
         all/0, get/2, find_by_schema_name/1,
         update/1, delete/2,
         checksum/1, size/0
        ]).

-define(TBL_NAME, ?TBL_COLUMNS).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create a table of system-configutation
%%
create_table(Mode, Nodes) ->
    mnesia:create_table(
      ?TBL_COLUMNS,
      [{Mode, Nodes},
       {type, set},
       {record_name, svdb_column},
       {attributes, record_info(fields, svdb_column)},
       {user_properties,
        [{id,           pos_integer, primary},
         {schema_name,  atom,        false  },
         {name,         atom,        false  },
         {type,         atom,        false  },
         {constraint,   list,        false  },
         {created_at,   pos_integer, false  }
        ]}
      ]).

%% @doc Retrieve all records
%%
-spec(all() ->
             {ok, [#svdb_column{}]} | not_found | {error, any()}).
all() ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve a schema by name
%%
-spec(get(svdb_schema(), svdb_key()) ->
             {ok, #svdb_column{}} | not_found | {error, any()}).
get(Schema, ColumnName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#svdb_column.schema_name == Schema,
                                        X#svdb_column.name == ColumnName]),
                        qlc:e(Q)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.

%% @doc Retrieve a schema by name
%%
-spec(find_by_schema_name(svdb_schema()) ->
             {ok, #svdb_column{}} | not_found | {error, any()}).
find_by_schema_name(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#svdb_column.schema_name == SchemaName]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, Values} ->
                    {ok, Values};
                Other ->
                    Other
            end
    end.


%% @doc Modify a schema
%%
-spec(update(#svdb_column{}) ->
             ok | {error, any()}).
update(Column) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun()-> mnesia:write(?TBL_NAME, Column, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Remove system-configuration
%%
-spec(delete(svdb_schema(), svdb_key()) ->
             ok | {error, any()}).
delete(Schema, ColumnName) ->
    case ?MODULE:get(Schema, ColumnName) of
        {ok, Column} ->
            Fun = fun() ->
                          mnesia:delete_object(?TBL_NAME, Column, write)
                  end,
            leo_mnesia:delete(Fun);
        Error ->
            Error
    end.


%% @doc Retrieve the checksum of this table
%%
-spec(checksum(svdb_schema()) ->
             integer() | {error, any()}).
checksum(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#svdb_column.schema_name == SchemaName]),
                        qlc:e(Q)
                end,
            case leo_mnesia:read(F) of
                {ok, Records} ->
                    erlang:phash2(Records);
                _ ->
                    -1
            end
    end.


%% @doc Retrieve the records
%%
-spec(size() ->
             pos_integer()).
size() ->
    mnesia:table_info(?TBL_NAME, size).
