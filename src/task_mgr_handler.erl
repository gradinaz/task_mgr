-module(task_mgr_handler).
-author("svasylchuk").

-include("rest.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
  init/2,
  allowed_methods/2,
  charsets_provided/2,
  content_types_accepted/2,
  post_json/2
]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

charsets_provided(Req, State) ->
  {[<<"utf-8">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, post_json}], Req, State}.

post_json(InitReq, State) ->
  task_mgr_util:handle_request(InitReq, State, fun(ApiReq, Req) ->
    Json = task_mgr_util:decode(ApiReq#api_req.body),
    case maps:get(<<"return_bash_script">>, Json, false) of
        true ->
          ScriptBin = task_mgr:get_bash_script(Json),
          prepare_response(ScriptBin, Req, State);
      false ->
        OrderedTasks = task_mgr:order_tasks(Json),
        prepare_response(#{<<"tasks">> => OrderedTasks}, Req, State)
    end
  end).


prepare_response(Response, Req, State) ->
  Body = task_mgr_util:encode(Response),
  % Setup the response.
  Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
  Req3 = cowboy_req:set_resp_body(Body, Req2),
  {true, Req3, State}.
