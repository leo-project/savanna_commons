%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Basho Technologies, Inc.
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
%% -------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File:      folsom_sample_slide_server.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc
%%% Serialization point for folsom_sample_slide. Handles
%%% pruning of older smaples. One started per histogram.
%%% See folsom.hrl, folsom_sample_slide, folsom_sample_slide_sup
%%% @end
%%%-----------------------------------------------------------------
-module(svc_sample_slide_server).

-behaviour(gen_server).

-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sample_mod,
                sample_server_id,
                window}).

-define(DEF_TIMEOUT, 60000).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(SampleMod, SmpleServerId, Window) ->
    gen_server:start_link(?MODULE, [SampleMod, SmpleServerId, Window], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([SampleMod, SampleServerId, Window]) ->
    {ok, #state{sample_mod = SampleMod,
                sample_server_id = SampleServerId,
                window = Window}, Window}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State=#state{sample_mod = SampleMod,
                                  sample_server_id = SampleSeverId,
                                  window = Window}) ->
    spawn(fun() ->
                  timer:sleep(erlang:phash2(leo_date:clock(), 250)),
                  catch SampleMod:trim_and_notify(SampleSeverId)
          end),
    {noreply, State, Window};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
