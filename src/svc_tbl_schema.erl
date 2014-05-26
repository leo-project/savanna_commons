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
-module(svc_tbl_schema).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([create_table/2,
         all/0,
         get/1, insert/1, update/1, delete/1,
         checksum/0, size/0,
         sync/1
        ]).

-define(TBL_NAME, ?TBL_SCHEMAS).


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
       {record_name, sv_schema},
       {attributes, record_info(fields, sv_schema)},
       {user_properties,
        [{name,       atom,        primary},
         {created_at, pos_integer, false  }
        ]}
      ]).

%% @doc Retrieve all records
%%
-spec(all() ->
             {ok, [#sv_schema{}]} | not_found | {error, any()}).
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
-spec(get(sv_schema()) ->
             {ok, #sv_schema{}} | not_found | {error, any()}).
get(SchemaName) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#sv_schema.name == SchemaName]),
                        qlc:e(Q)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Insert a schema
%%
-spec(insert(#sv_schema{}) ->
             ok | {error, any()}).
insert(#sv_schema{} = Schema) ->
    update(Schema).


%% @doc Modify a schema
%%
-spec(update(#sv_schema{}) ->
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
-spec(delete(sv_schema()) ->
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
            erlang:phash2(lists:sort(Records));
        _ ->
            -1
    end.


%% @doc Retrieve the records
%%
-spec(size() ->
             pos_integer()).
size() ->
    mnesia:table_info(?TBL_NAME, size).


%% @doc Synchronize the table
%%
-spec(sync(list(#sv_schema{})) ->
             ok | {error, any()}).
sync(SchemasMaster) ->
    case all() of
        {ok, SchemasLocal} ->
            ok = sync_1(SchemasMaster, SchemasLocal),
            ok = sync_2(SchemasLocal,  SchemasMaster);
        not_found ->
            sync_1(SchemasMaster);
        _ ->
            ok
    end.


%% @private
sync_1([]) ->
    ok;
sync_1([#sv_schema{} = Schema|Rest]) ->
    case update(Schema) of
        ok ->
            sync_1(Rest);
        {error, Cause} ->
            {error, Cause}
    end;
sync_1(_) ->
    {error, invalud_data_type}.


%% @private
sync_1([],_SchemasLocal) ->
    ok;
sync_1([#sv_schema{} = Schema|Rest], SchemasLocal) ->
    ok = sync_1_1(SchemasLocal, Schema),
    sync_1(Rest, SchemasLocal);
sync_1(_,_) ->
    {error, invalud_data_type}.

%% @private
sync_1_1([], SchemaMaster) ->
    _ = update(SchemaMaster),
    ok;
sync_1_1([#sv_schema{name = Name} = SchemaLocal|_], #sv_schema{name = Name} = SchemaMaster) ->
    case (SchemaLocal == SchemaMaster) of
        true ->
            void;
        false ->
            _ = update(SchemaMaster)
    end,
    ok;
sync_1_1([#sv_schema{}|Rest], SchemaMaster) ->
    sync_1_1(Rest, SchemaMaster).


%% @private
sync_2([],_SchemasMaster) ->
    ok;
sync_2([#sv_schema{} = Schema|Rest], SchemasMaster) ->
    ok = sync_2_1(SchemasMaster, Schema),
    sync_2(Rest, SchemasMaster).

sync_2_1([], #sv_schema{name = Name}) ->
    %% Remove unnecessary a col
    _ = delete(Name),
    ok;
sync_2_1([#sv_schema{name = Name}|_], #sv_schema{name = Name}) ->
    ok;
sync_2_1([#sv_schema{}|Rest], SchemaLocal) ->
    sync_2_1(Rest, SchemaLocal).
