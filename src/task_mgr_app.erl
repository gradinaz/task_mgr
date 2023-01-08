-module(task_mgr_app).
-author("svasylchuk").

-behaviour(application).

-export([
  start/0,
  stop/0
]).

%% Application callbacks
-export([start/2,
  stop/1]).

-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% API
%% ===================================================================

start() ->
  application:ensure_all_started(task_mgr).

stop() ->
  application:stop(task_mgr).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  RestDispatch = cowboy_router:compile([
    {'_', [
      {<<"/api/tasks">>,  task_mgr_handler, []}
    ]}
  ]),

  Middlewares = [cowboy_router, cowboy_handler],


  {ok, _} = cowboy:start_clear(task_mgr_http, [{port, 8084}],
    #{env => #{dispatch => RestDispatch},  middlewares => Middlewares}),

  task_mgr_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
