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
-author('Yosuke Hara').

-type(sv_metric() :: atom()).
-type(sv_schema() :: atom()).
-type(sv_key()    :: atom()).
-type(sv_keyval() :: {atom(), any()}).
-type(sv_values() :: list(tuple())).

-define(ERROR_ETS_NOT_AVAILABLE, "ETS is not available").
-define(ERROR_MNESIA_NOT_START,  "").

-define(METRIC_COUNTER,   'counter').
-define(METRIC_HISTOGRAM, 'histogram').
-define(METRIC_HISTORY,   'history').
-type(sv_metric_type() :: ?METRIC_COUNTER | ?METRIC_HISTOGRAM | ?METRIC_HISTORY).

-define(HISTOGRAM_UNIFORM,       'uniform').
-define(HISTOGRAM_EXDEC,         'exdec').
-define(HISTOGRAM_SLIDE,         'slide').
-define(HISTOGRAM_SLIDE_UNIFORM, 'slide_uniform').
-type(sv_histogram_type() :: ?HISTOGRAM_UNIFORM |
                             ?HISTOGRAM_EXDEC |
                             ?HISTOGRAM_SLIDE |
                             ?HISTOGRAM_SLIDE_UNIFORM).

-define(HISTOGRAM_CONS_SAMPLE, 'sample').
-define(HISTOGRAM_CONS_ALPHA,  'alpha').
-type(sv_histogram_constraint() :: ?HISTOGRAM_CONS_SAMPLE |
                                   ?HISTOGRAM_CONS_ALPHA).

-define(TBL_SCHEMAS, 'sv_schemas').
-define(TBL_COLUMNS, 'sv_columns').

-define(COL_TYPE_COUNTER,         'counter').
-define(COL_TYPE_H_UNIFORM,       'histogram_uniform').
-define(COL_TYPE_H_SLIDE,         'histogram_slide').
-define(COL_TYPE_H_SLIDE_UNIFORM, 'histogram_slide_uniform').
-define(COL_TYPE_H_EXDEC,         'histogram_exdec').
-define(COL_TYPE_HISTORY,         'history').
-type(sv_column_type() :: ?COL_TYPE_COUNTER |
                          ?COL_TYPE_H_UNIFORM |
                          ?COL_TYPE_H_SLIDE |
                          ?COL_TYPE_H_SLIDE_UNIFORM |
                          ?COL_TYPE_H_EXDEC |
                          ?COL_TYPE_HISTORY).

%% Macro
%% @doc Generate a metric-name from a schema-name and a key
-define(sv_metric_name(_Schema, _Key),
        list_to_atom(lists:append([atom_to_list(_Schema), ".", atom_to_list(_Key)]))).

%% @doc Retrieve a schema-name and a key from a metric-name
-define(sv_schema_and_key(_MetricName),
        begin
            _MetricName_1 = atom_to_list(_MetricName),
            _Index  = string:chr(_MetricName_1, $.),
            _Schema = list_to_atom(string:sub_string(_MetricName_1, 1, _Index-1)),
            _Key    = list_to_atom(string:sub_string(_MetricName_1, _Index + 1)),
            {_Schema,_Key}
        end).


%% Records
-record(sv_schema, {
          name       :: sv_schema(),
          created_at :: pos_integer()
         }).

-record(sv_column, {
          id              :: pos_integer(),
          schema_name     :: sv_schema(),
          name            :: sv_key(),
          type            :: sv_column_type(),
          constraint = [] :: list(),
          created_at      :: pos_integer()
         }).
