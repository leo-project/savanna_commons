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

-export([new/4, new/5, new/6, new/7, new/8,
         stop/2,
         create_schema/2,
         create_metrics_by_schema/3,
         notify/2, get_metric_value/2,
         get_histogram_statistics/2]).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API
%% ===================================================================
%% @doc Create a new metrics or histgram
%%
new(?METRIC_COUNTER, Schema, Key, Callback) ->
    Name = ?sv_metric_name(Schema, Key),
    svc_metrics_counter:start_link(Name, Callback).

new(?METRIC_COUNTER, Schema, Key, Window, Callback) ->
    Name = ?sv_metric_name(Schema, Key),
    svc_metrics_counter:start_link(Name, Window, Callback);

new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Callback) ->
    Name = ?sv_metric_name(Schema, Key),
    svc_metrics_histogram:start_link(Name, HistogramType, Callback).

new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Window, Callback) ->
    Name = ?sv_metric_name(Schema, Key),
    svc_metrics_histogram:start_link(Name, HistogramType, Window, Callback).

new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Window, SampleSize, Callback) ->
    Name = ?sv_metric_name(Schema, Key),
    svc_metrics_histogram:start_link(Name, HistogramType, Window, SampleSize, Callback).

new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Window, SampleSize, Alpha, Callback) ->
    Name = ?sv_metric_name(Schema, Key),
    svc_metrics_histogram:start_link(Name, HistogramType, Window, SampleSize, Alpha, Callback).


%% @doc Stop a process
stop(Schema, Key) ->
    Name = ?sv_metric_name(Schema, Key),
    case check_type(Name) of
        ?METRIC_COUNTER ->
            svc_metrics_counter:stop(Name);
        ?METRIC_HISTOGRAM ->
            svc_metrics_histogram:stop(Name);
        _ ->
            ok
    end.


%% doc Create a new metrics or histgram from the schema
%%
-spec(create_schema(sv_schema(), [#sv_column{}]) ->
             ok | {error, any()}).
create_schema(SchemaName, Columns) ->
    CreatedAt = leo_date:now(),
    case svc_tbl_schema:update(#sv_schema{name = SchemaName,
                                          created_at = CreatedAt}) of
        ok ->
            create_schema_1(SchemaName, Columns, CreatedAt);
        Error ->
            Error
    end.

%% @private
create_schema_1(_,[],_) ->
    ok;
create_schema_1(SchemaName, [#sv_column{} = Col|Rest], CreatedAt) ->
    Id = svc_tbl_column:size() + 1,
    case svc_tbl_column:update(Col#sv_column{id = Id,
                                             schema_name = SchemaName,
                                             created_at  = CreatedAt}) of
        ok ->
            create_schema_1(SchemaName, Rest, CreatedAt);
        Error ->
            Error
    end;
create_schema_1(_,_,_) ->
    {error, invalid_args}.


%% doc Create a new metrics or histgram by the schema
%%
-spec(create_metrics_by_schema(sv_schema(), pos_integer(), function()) ->
             ok | {error, any()}).
create_metrics_by_schema(SchemaName, Window, Callback) ->
    case svc_tbl_schema:get(SchemaName) of
        {ok,_} ->
            case svc_tbl_column:find_by_schema_name(SchemaName) of
                {ok, Columns} ->
                    create_metrics_by_schema_1(Columns, Window, Callback);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @private
create_metrics_by_schema_1([],_,_) ->
    ok;
create_metrics_by_schema_1([#sv_column{type = ?COL_TYPE_COUNTER,
                                       schema_name = Schema,
                                       name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_COUNTER, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#sv_column{type = ?COL_TYPE_H_UNIFORM,
                                       schema_name = Schema,
                                       constraint  = Constraint,
                                       name = Key}|Rest], Window, Callback) ->
    HType = ?HISTOGRAM_UNIFORM,
    {ok,_Pid} =
        case leo_misc:get_value(?HISTOGRAM_CONS_SAMPLE, Constraint, []) of
            [] -> new(?METRIC_HISTOGRAM, HType, Schema, Key, Window, Callback);
            N  -> new(?METRIC_HISTOGRAM, HType, Schema, Key, Window, N, Callback)
        end,
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#sv_column{type = ?COL_TYPE_H_SLIDE,
                                       schema_name = Schema,
                                       name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_HISTOGRAM, ?HISTOGRAM_SLIDE, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#sv_column{type = ?COL_TYPE_H_EXDEC,
                                       schema_name = Schema,
                                       constraint  = Constraint,
                                       name = Key}|Rest], Window, Callback) ->
    HType = ?HISTOGRAM_EXDEC,
    {ok,_Pid} =
        case leo_misc:get_value(?HISTOGRAM_CONS_SAMPLE, Constraint, []) of
            [] -> new(?METRIC_HISTOGRAM, HType, Schema, Key, Window, Callback);
            N1 ->
                case leo_misc:get_value(?HISTOGRAM_CONS_ALPHA, Constraint, []) of
                    [] -> new(?METRIC_HISTOGRAM, HType, Schema, Key, Window, N1, Callback);
                    N2 -> new(?METRIC_HISTOGRAM, HType, Schema, Key, Window, N1, N2, Callback)
                end
        end,

    %% {ok,_Pid} = new(?METRIC_HISTOGRAM, ?HISTOGRAM_EXDEC, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1(_,_,_) ->
    {error, invalid_args}.


%% @doc Notify an event with a schema and a key
%%
-spec(notify(sv_schema(), sv_keyval()) ->
             ok | {error, any()}).
notify(Schema, {Key, Event}) ->
    Name = ?sv_metric_name(Schema, Key),
    notify(check_type(Name), Name, Event).

%% @private
notify(?METRIC_COUNTER, Name, Event) ->
    folsom_metrics:notify({Name, {inc, Event}});
notify(?METRIC_HISTOGRAM, Name, Event) ->
    svc_metrics_histogram:update(Name, Event);
notify(_,_,_) ->
    {error, invalid_args}.


%% @doc Retrieve a metric value
%%
-spec(get_metric_value(sv_schema(), atom()) ->
             {ok, any()} | {error, any()}).
get_metric_value(Schema, Key) ->
    Name = ?sv_metric_name(Schema, Key),
    get_metric_value_1(check_type(Name), Name).

%% @private
get_metric_value_1(?METRIC_COUNTER, Name) ->
    svc_metrics_counter:get_values(Name);
get_metric_value_1(?METRIC_HISTOGRAM, Name) ->
    svc_metrics_histogram:get_values(Name);
get_metric_value_1(_,_) ->
    {error, invalid_args}.


%% @doc Retrieve a historgram statistics
%%
get_histogram_statistics(Schema, Key) ->
    Name = ?sv_metric_name(Schema, Key),
    case check_type(Name) of
        ?METRIC_HISTOGRAM ->
            svc_metrics_histogram:get_histogram_statistics(Name);
        _ ->
            not_found
    end.


%% ===================================================================
%% Inner Functions
%% ===================================================================
%% @private
check_type(Name) ->
    check_type([?METRIC_COUNTER, ?METRIC_HISTOGRAM], Name).

%% @private
check_type([],_Name) ->
    not_found;
check_type([?METRIC_COUNTER = Type|Rest], Name) ->
    case ets:lookup(?COUNTER_TABLE, {Name, 0}) of
        [{{Name, 0},0}|_] ->
            Type;
        _ ->
            check_type(Rest, Name)
    end;
check_type([?METRIC_HISTOGRAM = Type|Rest], Name) ->
    case ets:lookup(?HISTOGRAM_TABLE, Name) of
        [{Name,{histogram,_,_}}|_] ->
            Type;
        _Other ->
            check_type(Rest, Name)
    end.
