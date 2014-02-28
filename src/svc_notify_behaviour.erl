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
-module(svc_notify_behaviour).
-author('Yosuke Hara').

-include("savanna_commons.hrl").


%% @doc Notify metric(s) and statistics to a callback-mod
%%
-callback(notify(Schema::sv_schema(),
                 MetricGroup::sv_metric_grp(),
                 {Key::sv_key(), Values::sv_values()}) ->
                 ok | {error, any()}).
