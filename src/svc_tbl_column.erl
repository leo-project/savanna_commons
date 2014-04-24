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
-module(svc_tbl_column).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([create_table/2,
         all/0, get/2,
         find_by_id/1,
         find_by_schema_name/1,
         insert/1, update/1, delete/1, delete/2,
         checksum/0, checksum/1, size/0,
         sync/1,
         transform/0
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
       {record_name, ?SV_COLUMN},
       {attributes, record_info(fields, ?SV_COLUMN)},
       {user_properties,
        [{id,           tuple,       primary},
         {schema_name,  atom,        false  },
         {name,         atom,        false  },
         {type,         atom,        false  },
         {constraint,   list,        false  },
         {updated_at,   pos_integer, false  },
         {created_at,   pos_integer, false  },
         {del,          boolean,     false  }
        ]}
      ]).

%% @doc Retrieve all records
%%
-spec(all() ->
             {ok, [#?SV_COLUMN{}]} | not_found | {error, any()}).
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
-spec(get(sv_schema(), sv_key()) ->
             {ok,#?SV_COLUMN{}} | not_found | {error, any()}).
get(Schema, ColumnName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#?SV_COLUMN.schema_name == Schema,
                                        X#?SV_COLUMN.name == ColumnName,
                                        X#?SV_COLUMN.del == false
                                  ]),
                        qlc:e(Q)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Retrieve a schema by id
%%
-spec(find_by_id(pos_integer()) ->
             {ok,#?SV_COLUMN{}} | not_found | {error, any()}).
find_by_id(Id) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                         X#?SV_COLUMN.id == Id]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, [Val|_]} ->
                    {ok, Val};
                Other ->
                    Other
            end
    end.


%% @doc Retrieve a schema by name
%%
-spec(find_by_schema_name(sv_schema()) ->
             {ok,#?SV_COLUMN{}} | not_found | {error, any()}).
find_by_schema_name(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                         X#?SV_COLUMN.schema_name == SchemaName,
                                         X#?SV_COLUMN.del == false
                                   ]),
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
-spec(insert(#?SV_COLUMN{}) ->
             ok | {error, any()}).
insert(#?SV_COLUMN{} = Col) ->
    update(Col).


%% @doc Modify a schema
%%
-spec(update(#?SV_COLUMN{}) ->
             ok | {error, any()}).
update(#?SV_COLUMN{schema_name = Schema,
                   name = ColName} = Col) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            Now = leo_date:now(),
            Col_1 = case Col#?SV_COLUMN.created_at of
                        0 ->
                            Col#?SV_COLUMN{id = {Schema, ColName},
                                           updated_at = Now,
                                           created_at = Now};
                        _ ->
                            Col#?SV_COLUMN{id = {Schema, ColName},
                                           updated_at = Now}
                    end,
            F = fun()-> mnesia:write(?TBL_NAME, Col_1, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Remove system-configuration
%%
-spec(delete(pos_integer()) ->
             ok | {error, any()}).
delete(Id) ->
    case ?MODULE:find_by_id(Id) of
        {ok, Col} ->
            Fun = fun() ->
                          mnesia:delete_object(?TBL_NAME, Col, write)
                  end,
            leo_mnesia:delete(Fun);
        Error ->
            Error
    end.

-spec(delete(sv_schema(), sv_key()) ->
             ok | {error, any()}).
delete(Schema, ColName) ->
    case ?MODULE:get(Schema, ColName) of
        {ok, Col} ->
            update(Col#?SV_COLUMN{del = true});
        Error ->
            Error
    end.


%% @doc Retrieve the checksum of this table
%%
-spec(checksum() ->
             integer() | {error, any()}).
checksum() ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            case ?MODULE:all() of
                {ok, Records} ->
                    erlang:phash2(Records);
                _ ->
                    -1
            end
    end.


-spec(checksum(sv_schema()) ->
             integer() | {error, any()}).
checksum(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                         X#?SV_COLUMN.schema_name == SchemaName]),
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


%% @doc Synchronize the talble
%%
-spec(sync(list(#?SV_COLUMN{})) ->
             ok | {error, any()}).
sync(ColsMaster) ->
    case all() of
        {ok, ColsLocal} ->
            ok = sync_1(ColsMaster, ColsLocal),
            ok = sync_2(ColsLocal,  ColsMaster);
        not_found ->
            sync_1(ColsMaster);
        _ ->
            ok
    end.

%% @private
sync_1([]) ->
    ok;
sync_1([#?SV_COLUMN{} = Col|Rest]) ->
    case update(Col) of
        ok ->
            sync_1(Rest);
        {error, Cause} ->
            {error, Cause}
    end;
sync_1(_) ->
    {error, invalud_data_type}.


%% @private
sync_1([],_ColsLocal) ->
    ok;
sync_1([#?SV_COLUMN{} = Col|Rest], ColsLocal) ->
    ok = sync_1_1(ColsLocal, Col),
    sync_1(Rest, ColsLocal);
sync_1(_,_) ->
    {error, invalud_data_type}.

%% @private
sync_1_1([], ColMaster) ->
    _ = update(ColMaster),
    ok;
sync_1_1([#?SV_COLUMN{id = Id} = ColLocal|_], #?SV_COLUMN{id = Id} = ColMaster) ->
    case (ColLocal == ColMaster) of
        true ->
            void;
        false ->
            _ = update(ColMaster)
    end,
    ok;
sync_1_1([#?SV_COLUMN{}|Rest], ColMaster) ->
    sync_1_1(Rest, ColMaster).


%% @private
sync_2([],_ColsMaster) ->
    ok;
sync_2([#?SV_COLUMN{} = Col|Rest], ColsMaster) ->
    ok = sync_2_1(ColsMaster, Col),
    sync_2(Rest, ColsMaster);
sync_2(_,_) ->
    {error, invalud_data_type}.

sync_2_1([], #?SV_COLUMN{id = Id}) ->
    %% @TODO - remove unnecessary a col
    _ = delete(Id),
    ok;
sync_2_1([#?SV_COLUMN{id = Id}|_], #?SV_COLUMN{id = Id}) ->
    ok;
sync_2_1([#?SV_COLUMN{}|Rest], ColLocal) ->
    sync_2_1(Rest, ColLocal).


%% @doc Transform data
%%
-spec(transform() ->
             ok).
transform() ->
    {atomic, ok} = mnesia:transform_table(
                     ?TBL_NAME,
                     fun transform_1/1, record_info(fields, ?SV_COLUMN), ?SV_COLUMN),
    ok.

transform_1(#?SV_COLUMN{} = Col) ->
    Col;
transform_1(#sv_column{
               id = Id,
               schema_name = SchemaName,
               name = Name,
               type = Type,
               constraint = Cons,
               created_at = CreatedAt}) ->
    #?SV_COLUMN{id = Id,
                schema_name = SchemaName,
                name = Name,
                type = Type,
                constraint = Cons,
                updated_at = CreatedAt,
                created_at = CreatedAt,
                del = false}.
