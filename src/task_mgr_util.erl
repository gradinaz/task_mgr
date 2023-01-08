-module(task_mgr_util).
-author("svasylchuk").

%% API
-export([
  handle_request/3,
  encode/1,
  decode/1]).

-include("rest.hrl").


-spec handle_request(cowboy_req:req(), term(), fun()) ->
  {ok, Req} |
  {halt, Req, tuple()} when Req::cowboy_req:req().
handle_request(Req, State, Fun) ->
  QS = cowboy_req:parse_qs(Req),
  {Body, Req2} = read_body(Req),
  handle_errors(Req2, State, fun(Req3) ->
    % Prepare the API request structure, do authentication and QS checking.
    Fun(#api_req{qs=QS, body=Body}, Req3)
 end).

-spec handle_errors(cowboy_req:req(), term(), fun()) ->
  {ok, Req} |
  {halt, Req, term()} when
  Req::cowboy_req:req().
handle_errors(Req, State, Fun) ->
  try
    Fun(Req)
  catch
    {error, Code, Message} ->
      EBody = encode(#{error => Message}),
      EReq = cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>}, EBody, Req),
      {stop, EReq, State};

    {error, Code, Message, Data} ->
      EBody = encode(maps:merge(#{error => Message}, Data)),
      EReq = cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>}, EBody, Req),
      {stop, EReq, State}
  end.


-spec read_body(Req) -> {any(), Req} when Req::cowboy_req:req().
read_body(Req) ->
  case cowboy_req:read_body(Req) of
    {more, Data, Req2} ->
      {ContData, Req3} = read_body(Req2),
      {<<Data/binary, ContData/binary>>, Req3};

    {ok, Data, Req2} ->
      {Data, Req2}
  end.


-spec encode(term() ) -> binary().
encode(Term) ->
  jsone:encode(Term, [native_utf8, {indent, 2}, {space, 1}]).

-spec decode(binary() ) -> term().
decode(Bin) ->
  jsone:decode(Bin).

