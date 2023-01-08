-record(api_req, {
  qs,
  body,
  auth_data = []
}).

-type api_req() :: #api_req{}.