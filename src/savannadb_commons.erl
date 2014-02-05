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
-module(savannadb_commons).
-author('Yosuke Hara').

-export([new/4, new/5, new/6, new/7, new/8,
         create_schema/2,
         create_metrics_by_schema/3,
         notify/2, get_metric_value/2,
         get_histogram_statistics/2]).

-include("savannadb_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API
%% ===================================================================
%% @doc Create a new metrics or histgram
%%
new(?METRIC_COUNTER, Schema, Key, Callback) ->
    Name = ?svdb_metric_name(Schema, Key),
    svdbc_metrics_counter:start_link(Name, Callback).
new(?METRIC_COUNTER, Schema, Key, Window, Callback) ->
    Name = ?svdb_metric_name(Schema, Key),
    svdbc_metrics_counter:start_link(Name, Window, Callback);

new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Callback) ->
    Name = ?svdb_metric_name(Schema, Key),
    svdbc_sample_slide:start_link(Name, HistogramType, Callback).
new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Window, Callback) ->
    Name = ?svdb_metric_name(Schema, Key),
    svdbc_sample_slide:start_link(Name, HistogramType, Window, Callback).
new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Window, SampleSize, Callback) ->
    Name = ?svdb_metric_name(Schema, Key),
    svdbc_sample_slide:start_link(Name, HistogramType, Window, SampleSize, Callback).
new(?METRIC_HISTOGRAM, HistogramType, Schema, Key, Window, SampleSize, Alpha, Callback) ->
    Name = ?svdb_metric_name(Schema, Key),
    svdbc_sample_slide:start_link(Name, HistogramType, Window, SampleSize, Alpha, Callback).


%% doc Create a new metrics or histgram from the schema
%%
-spec(create_schema(svdb_schema(), [#svdb_column{}]) ->
             ok | {error, any()}).
create_schema(SchemaName, Columns) ->
    CreatedAt = leo_date:now(),
    case svdbc_tbl_schema:update(#svdb_schema{name = SchemaName,
                                              created_at = CreatedAt}) of
        ok ->
            create_schema_1(SchemaName, Columns, CreatedAt);
        Error ->
            Error
    end.

%% @private
create_schema_1(_,[],_) ->
    ok;
create_schema_1(SchemaName, [#svdb_column{} = Col|Rest], CreatedAt) ->
    Id = svdbc_tbl_column:size() + 1,
    case svdbc_tbl_column:update(Col#svdb_column{id = Id,
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
-spec(create_metrics_by_schema(svdb_schema(), pos_integer(), function()) ->
             ok | {error, any()}).
create_metrics_by_schema(SchemaName, Window, Callback) ->
    case svdbc_tbl_schema:get(SchemaName) of
        {ok,_} ->
            case svdbc_tbl_column:find_by_schema_name(SchemaName) of
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
create_metrics_by_schema_1([#svdb_column{type = ?COL_TYPE_COUNTER,
                                         schema_name = Schema,
                                         name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_COUNTER, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#svdb_column{type = ?COL_TYPE_H_UNIFORM,
                                         schema_name = Schema,
                                         name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_HISTOGRAM, ?HISTOGRAM_UNIFORM, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#svdb_column{type = ?COL_TYPE_H_SLIDE,
                                         schema_name = Schema,
                                         name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_HISTOGRAM, ?HISTOGRAM_SLIDE, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#svdb_column{type = ?COL_TYPE_H_SLIDE_UNIFORM,
                                         schema_name = Schema,
                                         name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_HISTOGRAM, ?HISTOGRAM_SLIDE_UNIFORM, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1([#svdb_column{type = ?COL_TYPE_H_EXDEC,
                                         schema_name = Schema,
                                         name = Key}|Rest], Window, Callback) ->
    {ok,_Pid} = new(?METRIC_HISTOGRAM, ?HISTOGRAM_EXDEC, Schema, Key, Window, Callback),
    create_metrics_by_schema_1(Rest, Window, Callback);
create_metrics_by_schema_1(_,_,_) ->
    {error, invalid_args}.


%% @doc Notify an event with a schema and a key
%%
-spec(notify(svdb_schema(), svdb_keyval()) ->
             ok | {error, any()}).
notify(Schema, {Key, Event}) ->
    Name = ?svdb_metric_name(Schema, Key),
    notify(check_type(Name), Name, Event).

%% @private
notify(?METRIC_COUNTER, Name, Event) ->
    folsom_metrics:notify({Name, Event});
notify(?METRIC_HISTOGRAM, Name, Event) ->
    svdbc_sample_slide:update(Name, Event);
notify(_,_,_) ->
    {error, invalid_args}.


%% @doc Retrieve a metric value
%%
-spec(get_metric_value(svdb_schema(), atom()) ->
             {ok, any()} | {error, any()}).
get_metric_value(Schema, Key) ->
    Name = ?svdb_metric_name(Schema, Key),
    get_metric_value_1(check_type(Name), Name).

%% @private
get_metric_value_1(?METRIC_COUNTER, Name) ->
    svdbc_metrics_counter:get_values(Name);
get_metric_value_1(?METRIC_HISTOGRAM, Name) ->
    svdbc_sample_slide:get_values(Name);
get_metric_value_1(_,_) ->
    {error, invalid_args}.


%% @doc Retrieve a historgram statistics
%%
get_histogram_statistics(Schema, Key) ->
    Name = ?svdb_metric_name(Schema, Key),
    case check_type(Name) of
        ?METRIC_HISTOGRAM ->
            svdbc_sample_slide:get_histogram_statistics(Name);
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
    case ets:lookup(?SPIRAL_TABLE, Name) of
        [{Name,{spiral,_,_}}|_] ->
            Type;
        _ ->
            check_type(Rest, Name)
    end;
check_type([?METRIC_HISTOGRAM = Type|Rest], Name) ->
    case ets:lookup(?HISTOGRAM_TABLE, Name) of
        [{Name,{histogram,_,{_,_,_,_}}}|_] ->
            Type;
        _ ->
            check_type(Rest, Name)
    end.
