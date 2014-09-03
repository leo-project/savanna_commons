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
-module(savanna_commons).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([new/1,
         create_schema/2,
         create_metrics_by_schema/3,
         create_metrics_by_schema/4,
         create_metrics_by_schema/5,
         notify/2, get_metric_value/2,
         get_histogram_statistics/2,
         sync_schemas/1,
         sync_tables/2,
         checksum/0
        ]).


%% ===================================================================
%% API
%% ===================================================================
%% @doc Create a new metrics or histgram
%%
new(#sv_metric_conf{metric_type = ?METRIC_COUNTER,
                    metric_group_name = MetricGroup,
                    name = Key} = MetricConf) ->
    ServerId = ?sv_metric_name(MetricGroup, Key),
    Ret = savanna_commons_sup:start_child(?MOD_METRICS_COUNTER, ServerId, MetricConf),
    new_1(Ret);

new(#sv_metric_conf{metric_type = ?METRIC_GAUGE,
                    metric_group_name = MetricGroup,
                    name = Key} = MetricConf) ->
    ServerId = ?sv_metric_name(MetricGroup, Key),
    Ret = savanna_commons_sup:start_child(?MOD_METRICS_GAUGE, ServerId, MetricConf),
    new_1(Ret);

new(#sv_metric_conf{metric_type = ?METRIC_HISTOGRAM,
                    metric_group_name = MetricGroup,
                    name = Key} = MetricConf) ->
    ServerId = ?sv_metric_name(MetricGroup, Key),
    Ret = savanna_commons_sup:start_child(?MOD_METRICS_HISTOGRAM, ServerId, MetricConf),
    new_1(Ret).


%% @private
new_1(ok) ->
    ok;
new_1({error,{already_started,_Pid}}) ->
    ok;
new_1(Error) ->
    Error.


%% doc Create a new metrics or histgram from the schema
%%
-spec(create_schema(sv_schema(), [#?SV_COLUMN{}]) ->
             ok | {error, any()}).
create_schema(SchemaName, Columns) ->
    SchemaName_1 = leo_misc:any_to_binary(SchemaName),
    CreatedAt = leo_date:now(),

    case svc_tbl_schema:update(#?SV_SCHEMA{name = SchemaName_1,
                                           created_at = CreatedAt}) of
        ok ->
            create_schema_1(SchemaName_1, Columns, CreatedAt);
        {error, Cause} ->
            {error, Cause}
    end.

%% @private
-spec(create_schema_1(sv_schema(), [_], non_neg_integer()) ->
             ok | {error, any()}).
create_schema_1(_,[],_) ->
    ok;
create_schema_1(SchemaName, [#?SV_COLUMN{} = Col|Rest], CreatedAt) ->
    SchemaName_1 = leo_misc:any_to_binary(SchemaName),

    case svc_tbl_column:update(Col#?SV_COLUMN{schema_name = SchemaName_1,
                                              created_at  = CreatedAt}) of
        ok ->
            create_schema_1(SchemaName, Rest, CreatedAt);
        {error, Cause} ->
            {error, Cause}
    end;
create_schema_1(_,_,_) ->
    {error, invalid_args}.


%% doc Create a new metrics or histgram by the schema
%%
-spec(create_metrics_by_schema(sv_schema(), sv_metric_grp(), atom()) ->
             ok | not_found | {error, any()}).
create_metrics_by_schema(SchemaName, MetricGroupName, CallbackMod) ->
    create_metrics_by_schema(SchemaName, MetricGroupName, ?SV_WINDOW_1M, CallbackMod).

-spec(create_metrics_by_schema(sv_schema(), sv_metric_grp(),
                               integer(), atom()) ->
             ok | not_found | {error, any()}).
create_metrics_by_schema(SchemaName, MetricGroupName, Window, CallbackMod) ->
    create_metrics_by_schema(SchemaName, MetricGroupName, Window, ?SV_STEP_1M, CallbackMod).

-spec(create_metrics_by_schema(sv_schema(), sv_metric_grp(),
                               integer(), integer(), atom()) ->
             ok | not_found | {error, any()}).
create_metrics_by_schema(SchemaName, MetricGroupName, Window, Step, CallbackMod) ->
    SchemaName_1      = leo_misc:any_to_binary(SchemaName),
    MetricGroupName_1 = leo_misc:any_to_binary(MetricGroupName),

    case svc_tbl_schema:get(SchemaName_1) of
        {ok,_} ->
            case svc_tbl_column:find_by_schema_name(SchemaName_1) of
                {ok, Columns} ->
                    case svc_tbl_metric_group:update(
                           #sv_metric_group{schema_name = SchemaName_1,
                                            name     = MetricGroupName_1,
                                            window   = Window,
                                            step     = Step,
                                            callback = CallbackMod}) of
                        ok ->
                            create_metrics_by_schema_1(
                              MetricGroupName_1, Columns, Window, Step, CallbackMod);
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @private
-spec(create_metrics_by_schema_1(atom(),[#?SV_COLUMN{}], integer(), integer(), atom()) ->
             ok | {error, any()}).
create_metrics_by_schema_1(_,[],_,_,_) ->
    ok;
create_metrics_by_schema_1(MetricGroupName, [#?SV_COLUMN{type = ?COL_TYPE_COUNTER,
                                                         name = Key}|Rest], Window, Step, CallbackMod) ->
    MetricGroupName_1 = leo_misc:any_to_binary(MetricGroupName),
    ok = new(#sv_metric_conf{metric_type = ?METRIC_COUNTER,
                             metric_group_name = MetricGroupName_1,
                             name = Key,
                             window = Window,
                             callback = CallbackMod}),
    create_metrics_by_schema_1(MetricGroupName_1, Rest, Window, Step, CallbackMod);

create_metrics_by_schema_1(MetricGroupName, [#?SV_COLUMN{type = ?COL_TYPE_GAUGE,
                                                         name = Key}|Rest], Window, Step, CallbackMod) ->
    MetricGroupName_1 = leo_misc:any_to_binary(MetricGroupName),
    ok = new(#sv_metric_conf{metric_type = ?METRIC_GAUGE,
                             metric_group_name = MetricGroupName_1,
                             name = Key,
                             window = Window,
                             callback = CallbackMod}),
    create_metrics_by_schema_1(MetricGroupName_1, Rest, Window, Step, CallbackMod);

create_metrics_by_schema_1(MetricGroupName, [#?SV_COLUMN{type = ?COL_TYPE_H_UNIFORM,
                                                         constraint  = Constraint,
                                                         name = Key}|Rest], Window, Step, CallbackMod) ->
    MetricGroupName_1 = leo_misc:any_to_binary(MetricGroupName),
    HType = ?HISTOGRAM_UNIFORM,
    SampleSize = leo_misc:get_value(?HISTOGRAM_CONS_SAMPLE, Constraint, ?DEFAULT_SIZE),

    ok = new(#sv_metric_conf{metric_type = ?METRIC_HISTOGRAM,
                             histogram_type = HType,
                             metric_group_name = MetricGroupName_1,
                             name = Key,
                             window = Window,
                             sample_size = SampleSize,
                             callback = CallbackMod}),
    create_metrics_by_schema_1(MetricGroupName_1, Rest, Window, Step, CallbackMod);

create_metrics_by_schema_1(MetricGroupName, [#?SV_COLUMN{type = ?COL_TYPE_H_SLIDE,
                                                         name = Key}|Rest], Window, Step, CallbackMod) ->
    MetricGroupName_1 = leo_misc:any_to_binary(MetricGroupName),
    HType = ?HISTOGRAM_SLIDE,
    ok = new(#sv_metric_conf{metric_type = ?METRIC_HISTOGRAM,
                             histogram_type = HType,
                             metric_group_name = MetricGroupName_1,
                             name = Key,
                             window = Window,
                             callback = CallbackMod}),
    create_metrics_by_schema_1(MetricGroupName_1, Rest, Window, Step, CallbackMod);

create_metrics_by_schema_1(MetricGroupName, [#?SV_COLUMN{type = ?COL_TYPE_H_EXDEC,
                                                         constraint  = Constraint,
                                                         name = Key}|Rest], Window, Step, CallbackMod) ->
    MetricGroupName_1 = leo_misc:any_to_binary(MetricGroupName),
    HType = ?HISTOGRAM_EXDEC,
    SampleSize = leo_misc:get_value(?HISTOGRAM_CONS_SAMPLE, Constraint, ?DEFAULT_SIZE),
    AlphaValue = leo_misc:get_value(?HISTOGRAM_CONS_ALPHA,  Constraint, ?DEFAULT_ALPHA),

    ok = new(#sv_metric_conf{metric_type = ?METRIC_HISTOGRAM,
                             histogram_type = HType,
                             metric_group_name = MetricGroupName_1,
                             name = Key,
                             window = Window,
                             sample_size = SampleSize,
                             alpha = AlphaValue,
                             callback = CallbackMod}),
    create_metrics_by_schema_1(MetricGroupName_1, Rest, Window, Step, CallbackMod);
create_metrics_by_schema_1(_,_,_,_,_) ->
    {error, invalid_args}.


%% @doc Notify an event with a schema and a key
%%
-spec(notify(sv_metric_grp(), sv_keyval()) ->
             ok | {error, any()}).
notify(MetricGroup, {Key, Event}) ->
    notify(MetricGroup, {Key, Event}, 0).

notify(_,_,3) ->
    {error, "Could not access the metric-server"};
notify(MetricGroup, {Key, Event}, Times) ->
    ServerId = ?sv_metric_name(MetricGroup, Key),
    case catch svc_metric_server:update(ServerId, Event) of
        ok ->
            ok;
        _ ->
            _ = check_type(ServerId),
            case whereis(ServerId) of
                undefined -> {error, undefined};
                _ ->
                    notify(MetricGroup, {Key, Event}, Times + 1)
            end
    end.


%% @doc Retrieve a metric value
%%
-spec(get_metric_value(sv_metric_grp(), atom()) ->
             {ok, any()} | {error, any()}).
get_metric_value(MetricGroup, Key) ->
    ServerId = ?sv_metric_name(MetricGroup, Key),
    case catch svc_metric_server:get_values(ServerId) of
        {ok, Value} ->
            {ok, Value};
        _ ->
            _ = check_type(ServerId),
            not_found
    end.


%% @doc Retrieve a historgram statistics
%%
get_histogram_statistics(MetricGroup, Key) ->
    ServerId = ?sv_metric_name(MetricGroup, Key),
    case catch svc_metric_server:get_histogram_statistics(ServerId) of
        {ok, Value} ->
            {ok, Value};
        _ ->
            _ = check_type(ServerId),
            not_found
    end.


%% @doc Synchronize the schemas
%%
-spec(sync_schemas(list(atom())) ->
             ok | {error, any()}).
sync_schemas(Managers) ->
    sync_tbl_schema(Managers).


%% @doc Synchronize the tables
%%
-spec(sync_tables(list(#?SV_SCHEMA{}), list(#?SV_COLUMN{})) ->
             {ok, {integer(), integer()}} | {error, any()}).
sync_tables(Schemas, Columns) ->
    ok = svc_tbl_schema:sync(Schemas),
    ok = svc_tbl_column:sync(Columns),
    checksum().


%% @doc Retrieve checksum of each table
-spec(checksum() ->
             {ok, {integer(), integer()}}).
checksum() ->
    TblSchema = svc_tbl_schema:checksum(),
    TblColumn = svc_tbl_column:checksum(),
    {ok, {TblSchema, TblColumn}}.


%% ===================================================================
%% Inner Functions
%% ===================================================================
%% @private
-spec(check_type(atom()) ->
             {ok, atom()} | {error, any()}).
check_type(ServerId) ->
    case check_type([?METRIC_COUNTER,
                     ?METRIC_GAUGE,
                     ?METRIC_HISTOGRAM], ServerId) of
        not_found ->
            check_type_1(ServerId, undefined);
        Type ->
            case whereis(ServerId) of
                undefined ->
                    check_type_1(ServerId, Type);
                _Pid ->
                    Type
            end
    end.

check_type([],_ServerId) ->
    not_found;
check_type([?METRIC_COUNTER = Type|Rest], ServerId) ->
    case ets:lookup(?COUNTER_TABLE, {ServerId, 0}) of
        [{{ServerId, 0},0}|_] ->
            Type;
        _ ->
            check_type(Rest, ServerId)
    end;
check_type([?METRIC_GAUGE = Type|Rest], ServerId) ->
    case ets:lookup(?GAUGE_TABLE, {ServerId, 0}) of
        [{{ServerId, 0},0}|_] ->
            Type;
        _ ->
            check_type(Rest, ServerId)
    end;
check_type([?METRIC_HISTOGRAM = Type|Rest], ServerId) ->
    case ets:lookup(?HISTOGRAM_TABLE, ServerId) of
        [{ServerId,{histogram,_,_}}|_] ->
            Type;
        _Other ->
            check_type(Rest, ServerId)
    end.

%% @private
check_type_1(ServerId, Type) ->
    %% If retrieved a metric-group-info,
    %% then it will generate metrics
    {MetricGroup, Column} = ?sv_schema_and_key(ServerId),
    case svc_tbl_metric_group:get(MetricGroup) of
        {ok, #sv_metric_group{schema_name = Schema,
                              name = MetricGroup,
                              window = Window,
                              callback = CallbackMod}} ->
            case create_metrics_by_schema(
                   Schema, MetricGroup, Window, CallbackMod) of
                ok when Type =/= undefind ->
                    {ok, Type};
                ok ->
                    case svc_tbl_column:get(Schema, Column) of
                        {ok,#?SV_COLUMN{type = Type}} ->
                            {ok, Type};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, not_found}
    end.


%% @doc Synchronize schema-table
%% @private
sync_tbl_schema([]) ->
    ok;
sync_tbl_schema([Node|Rest]) ->
    case leo_rpc:call(Node, svc_tbl_schema, all, []) of
        {ok, Schemas} ->
            case update_tbl_schema(Schemas) of
                ok -> Schemas;
                _ -> []
            end;
        _ ->
            sync_tbl_schema(Rest)
    end.


%% @doc Update schema-table
%% @private
update_tbl_schema([]) ->
    ok;
update_tbl_schema([Schema|Rest]) ->
    case svc_tbl_schema:update(Schema) of
        ok ->
            update_tbl_schema(Rest);
        Error ->
            Error
    end.
