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
-module(svdbc_metrics_counter).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("savannadb_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2, start_link/3,
         stop/1]).

-export([get_values/1,
         trim/3
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name :: atom(),
                window = 0  :: pos_integer(),
                server      :: pid(),
                before = 0  :: pos_integer(),
                callback    :: atom() %% see:'svdbc_notify_behaviour'
               }).

-define(DEF_WINDOW, 10).
-define(DEF_WIDTH,  16).
-define(DEF_TIMEOUT, 30000).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Name, Callback) ->
    start_link(Name, ?DEF_WINDOW, Callback).

start_link(Name, Window, Callback) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Window, Callback], []).

stop(Name) ->
    gen_server:call(Name, stop).


%% @doc
-spec(get_values(atom()) ->
             {ok, tuple()} | {error, any()}).
get_values(Name) ->
    gen_server:call(Name, get_values, ?DEF_TIMEOUT).


%% @doc
-spec(trim(atom(), atom(), pos_integer()) ->
             ok | {error, any()}).
trim(Name, Tid, Window) ->
    gen_server:call(Name, {trim, Tid, Window}, ?DEF_TIMEOUT).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Name, Window, Callback]) ->
    Pid = svdbc_sup:start_slide_server(?MODULE, Name, -1, Window),
    ok = folsom_ets:add_handler(counter, Name),
    {ok, #state{name = Name,
                window = Window,
                server = Pid,
                callback  = Callback}}.

handle_call(stop, _From, #state{server = _Pid} = State) ->
    ok = svdbc_sample_slide_server:stop(_Pid),
    {stop, shutdown, ok, State};


handle_call(get_values, _From, #state{name = Name} = State) ->
    %% Reply = get_values_1(Tid, Window),
    Count = folsom_metrics_counter:get_value(Name),
    {reply, {ok, Count}, State};

handle_call({trim,_Tid, Window}, _From, #state{name = Name,
                                               callback = Callback,
                                               window = Window} = State) ->
    %% Retrieve the current value, then execute the callback-function
    ThisTime = folsom_utils:now_epoch(),
    Count = folsom_metrics_counter:get_value(Name),

    case is_atom(Callback) of
        true ->
            {SchemaName, Key} = ?svdb_schema_and_key(Name),
            catch Callback:notify(SchemaName, {Key, Count});
        false ->
            void
    end,

    %% Clear oldest data
    folsom_metrics_counter:dec(Name, Count),
    {reply, ok, State#state{before = ThisTime}}.


handle_cast(_Msg, State) ->
    {noreply, State}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason,_State) ->
    ok.


%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
