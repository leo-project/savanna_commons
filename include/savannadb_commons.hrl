%%======================================================================
%%
%% LeoProject - SavannaDB
%%
%% Copyright (c) 2013-2014 Rakuten, Inc.
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

-type(svdb_schema() :: atom()).
-type(svdb_key()    :: atom()).
-type(svdb_keyval() :: {atom(), any()}).

-define(ERROR_ETS_NOT_AVAILABLE, "ETS is not available").
-define(ERROR_MNESIA_NOT_START,  "").

-define(METRIC_COUNTER,   'counter').
-define(METRIC_HISTOGRAM, 'histogram').
-define(METRIC_HISTORY,   'history').
-type(svdb_metric_type() :: ?METRIC_COUNTER | ?METRIC_HISTOGRAM | ?METRIC_HISTORY).

-define(HISTOGRAM_UNIFORM,       'uniform').
-define(HISTOGRAM_EXDEC,         'exdec').
-define(HISTOGRAM_SLIDE,         'slide').
-define(HISTOGRAM_SLIDE_UNIFORM, 'slide_uniform').
-type(svdb_histogram_type() :: ?HISTOGRAM_UNIFORM |
                               ?HISTOGRAM_EXDEC |
                               ?HISTOGRAM_SLIDE |
                               ?HISTOGRAM_SLIDE_UNIFORM).

-define(TBL_SCHEMAS, 'svdb_schemas').
-define(TBL_COLUMNS, 'svdb_columns').

-define(COL_TYPE_COUNTER,         'counter').
-define(COL_TYPE_H_UNIFORM,       'histogram_uniform').
-define(COL_TYPE_H_SLIDE,         'histogram_slide').
-define(COL_TYPE_H_SLIDE_UNIFORM, 'histogram_slide_uniform').
-define(COL_TYPE_H_EXDEC,         'histogram_exdec').
-define(COL_TYPE_HISTORY,         'history').
-type(svdb_column_type() :: ?COL_TYPE_COUNTER |
                            ?COL_TYPE_H_UNIFORM |
                            ?COL_TYPE_H_SLIDE |
                            ?COL_TYPE_H_SLIDE_UNIFORM |
                            ?COL_TYPE_H_EXDEC |
                            ?COL_TYPE_HISTORY).

-record(svdb_schema, {
          name       :: svdb_schema(),
          created_at :: pos_integer()
         }).

-record(svdb_column, {
          id              :: pos_integer(),
          schema_name     :: svdb_schema(),
          name            :: svdb_key(),
          type            :: svdb_column_type(),
          constraint = [] :: list(),
          created_at      :: pos_integer()
         }).
