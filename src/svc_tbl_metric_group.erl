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
-module(svc_tbl_metric_group).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([create_table/2,
         all/0,
         get/1, find_by_schema_name/1,
         insert/1, update/1, delete/1,
         checksum/1, size/0
        ]).

-define(TBL_NAME, ?TBL_METRIC_GRP).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create a table of system-configutation
%%
create_table(Mode, Nodes) ->
    mnesia:create_table(
      ?TBL_NAME,
      [{Mode, Nodes},
       {type, set},
       {record_name, sv_metric_group},
       {attributes, record_info(fields, sv_metric_group)},
       {user_properties,
        [
         {id,           tuple,    primary},
         {schema_name,  binary,   false  },
         {name,         binary,   false  },
         {window,       integer,  false  },
         {callback,     atom,     false  },
         {created_at,   integer,  false  }
        ]}
      ]).

%% @doc Retrieve all records
%%
-spec(all() ->
             {ok, [#sv_metric_group{}]} | not_found | {error, any()}).
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
-spec(get(sv_metric_grp()) ->
             {ok, #sv_metric_group{}} | not_found | {error, any()}).
get(MetricGroupName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#sv_metric_group.name == MetricGroupName]),
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
-spec(find_by_schema_name(sv_schema()) ->
             {ok, #sv_metric_group{}} | not_found | {error, any()}).
find_by_schema_name(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                         X#sv_metric_group.schema_name == SchemaName]),
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


%% @doc Insert a schema
%%
-spec(insert(#sv_metric_group{}) ->
             ok | {error, any()}).
insert(#sv_metric_group{} = MetricGroup) ->
    update(MetricGroup).


%% @doc Modify a schema
%%
-spec(update(#sv_metric_group{}) ->
             ok | {error, any()}).
update(#sv_metric_group{schema_name = SchemaName,
                        name = MetricGroupName} = MetricGroup) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            MetricGroup_1 =
                case MetricGroup#sv_metric_group.created_at of
                    undefined ->
                        MetricGroup#sv_metric_group{
                          id = {SchemaName, MetricGroupName},
                          created_at = leo_date:now()};
                    _ ->
                        MetricGroup#sv_metric_group{
                          id = {SchemaName, MetricGroupName}}
                end,
            F = fun()-> mnesia:write(?TBL_NAME, MetricGroup_1, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Remove a schema
%%
-spec(delete(sv_metric_grp()) ->
             ok | {error, any()}).
delete(MetricGroupName) ->
    case ?MODULE:get(MetricGroupName) of
        {ok, MetricGroup} ->
            Fun = fun() ->
                          mnesia:delete_object(?TBL_NAME, MetricGroup, write)
                  end,
            leo_mnesia:delete(Fun);
        Error ->
            Error
    end.


%% @doc Retrieve the checksum of this table
%%
-spec(checksum(sv_schema()) ->
             integer()).
checksum(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                         X#sv_metric_group.schema_name == SchemaName]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
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
