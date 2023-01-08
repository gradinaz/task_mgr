-module(task_mgr).
-author("svasylchuk").

%% API
-export([order_tasks/1,
         get_bash_script/1]).


-spec order_tasks(Data::map()) -> [map()].
order_tasks(Data) ->
  Tasks = maps:get(<<"tasks">>, Data, []),
  Res = lists:foldl(fun(Task, FinAcc) ->
                        case maps:get(<<"requires">>, Task, []) of
                          [] ->  [Task];
                          Requires  -> get_needed_tasks(Tasks, Task, Requires, FinAcc)
                        end
                    end, [], Tasks),
  lists:reverse(clear_the_list(Res)).


-spec get_bash_script(Data::map()) -> binary().
get_bash_script(Data) ->
  OrderedTasks = order_tasks(Data),
  Shebang = maps:get(<<"shebang">>, Data, <<"#!/usr/bin/env_bash">>),
  generate_script(Shebang, OrderedTasks).


-spec generate_script(Shebang::binary(), OrderedTasks::[map()]) -> binary().
generate_script(Shebang, OrderedTasks) ->
  Commands = lists:foldl(fun(#{<<"command">> := Command}, Acc) ->
                            [Command | Acc]
                         end, [], OrderedTasks),
  list_to_binary(lists:join(<<"/n">>, [Shebang | Commands])).

-spec get_needed_tasks(ListsOfTasks::[map()], Task::map(), Requires::[binary()], FinAcc::[binary()]) -> [map()].
get_needed_tasks(ListsOfTasks, Task, Requires, FinAcc) ->
  case check_if_the_needed_task_is_in_the_final_list(FinAcc, Requires) of
    true ->
      maybe_add_task(FinAcc, Task);
    false ->
      NT = pick_needed_tasks(ListsOfTasks, Requires),
      NewTasks = [Task | NT],
      lists:flatten([NewTasks | FinAcc])
  end.

maybe_add_task(FinAcc, Task) ->
  case check_if_the_needed_task_is_in_the_final_list(FinAcc, [maps:get(<<"name">>, Task)]) of
    true -> FinAcc;
    false ->  [Task | FinAcc]
  end.

-spec pick_needed_tasks(ListsOfTasks::[map()], Requires::[binary()]) -> [map()].
pick_needed_tasks(ListsOfTasks, Requires) ->
  pick_needed_tasks(ListsOfTasks, Requires, []).
pick_needed_tasks(_ListsOfTasks, [], Acc) ->
  Acc;
pick_needed_tasks(ListsOfTasks, Requires, Acc) ->
  ReqT = lists:last(Requires),
  case lists:search(fun(#{<<"name">> := Name}) -> Name =:= ReqT end, ListsOfTasks) of
      false -> pick_needed_tasks(ListsOfTasks, Requires -- [ReqT], Acc);
      {value, Value} -> pick_needed_tasks(ListsOfTasks, Requires -- [ReqT], [Value | Acc])
  end.

-spec check_if_the_needed_task_is_in_the_final_list(List::[map()], Requires::[binary()]) -> boolean().
check_if_the_needed_task_is_in_the_final_list([], _Requires) ->
  false;
check_if_the_needed_task_is_in_the_final_list(List, Requires) ->
  check_if_the_needed_task_is_in_the_final_list(List, Requires, false).

check_if_the_needed_task_is_in_the_final_list(_List, [], Acc) ->
  Acc;
check_if_the_needed_task_is_in_the_final_list(List, Requires, _Acc) ->
  ReqT = lists:last(Requires),
  check_if_the_needed_task_is_in_the_final_list(List, Requires -- [ReqT], is_needed_task(List, ReqT)).

-spec is_needed_task(List::[map()], ReqTask::binary()) -> boolean().
is_needed_task(List, ReqTask) ->
  lists:any(fun(#{<<"name">> := Name}) -> Name =:= ReqTask end, List).

-spec clear_the_list(List::[map()]) -> [map()].
clear_the_list(List) ->
  lists:map(fun(Map) -> maps:remove(<<"requires">>, Map) end, List).
