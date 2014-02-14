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
-module(svc_metrics_counter).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/4,
         stop/1]).

-export([get_status/1,
         get_values/1,
         trim_and_notify/1
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name :: atom(),
                type = ?METRIC_COUNTER,
                window = 0 :: pos_integer(),
                callback   :: atom(), %% see:'svc_notify_behaviour'
                server     :: atom(),

                %% after this counter was over threshold of removal proc,
                %% then the server-proc will be removed
                counter = 0 :: pos_integer()
               }).

-define(DEF_TIMEOUT, 30000).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Name, Window, Callback, Server) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Window, Callback, Server], []).

stop(Name) ->
    gen_server:call(Name, stop).


%% @doc Retrieve current status
-spec(get_status(atom()) ->
             {ok, list(tuple())}).
get_status(Name) ->
    gen_server:call(Name, get_status, ?DEF_TIMEOUT).


%% @doc Retrieve current values
-spec(get_values(atom()) ->
             {ok, tuple()} | {error, any()}).
get_values(Name) ->
    gen_server:call(Name, get_values, ?DEF_TIMEOUT).

%% @doc Remove oldest values and notify metric with callback-func
-spec(trim_and_notify(atom()) ->
             ok | {error, any()}).
trim_and_notify(Name) ->
    gen_server:call(Name, trim_and_notify, ?DEF_TIMEOUT).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Name, Window, Callback, Server]) ->
    _ = folsom_ets:add_handler(counter, Name),
    {ok, #state{name = Name,
                window = Window,
                callback = Callback,
                server = Server
               }}.

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};


handle_call(get_status, _From, #state{name = Name,
                                      type = Type,
                                      window = Window,
                                      callback = Callback} = State) ->
    Reply = [{'name', Name}, {'type', Type}, {'window', Window},
             {'callback', Callback}],
    {reply, {ok, Reply}, State};

handle_call(get_values, _From, #state{name = Name} = State) ->
    Count = folsom_metrics_counter:get_value(Name),
    {reply, {ok, Count}, State};

handle_call(trim_and_notify, _From, State) ->
    NewState = trim_and_notify_1(State),
    {reply, ok, NewState}.


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
%% @private
trim_and_notify_1(#state{name = Name,
                         callback = Callback,
                         server = Server,
                         counter  = Counter} = State) ->
    %% Retrieve the current value, then execute the callback-function
    case folsom_metrics_counter:get_value(Name) of
        0 when Counter =< ?SV_THRESHOLD_OF_REMOVAL_PROC ->
            %% Terminate the server-proc
            timer:apply_after(100, savanna_commons_sup, stop_slide_server, [[Name, Server]]),
            State#state{counter = Counter + 1};
        0 ->
            State#state{counter = Counter + 1};
        Count ->
            case is_atom(Callback) of
                true ->
                    {SchemaName, Key} = ?sv_schema_and_key(Name),
                    catch Callback:notify(SchemaName, {Key, Count});
                false ->
                    void
            end,

            %% Clear oldest data
            folsom_metrics_counter:clear(Name),
            State#state{counter = 0}
    end.
