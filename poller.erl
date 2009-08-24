-module(poller).
-author('olivier@biniou.info').

-include("ss.hrl").
-include("loadavg.hrl").

-export([start/0, data/0]).

-record(state, {nodes, dict=dict:new()}).


start() ->
    register(?MODULE, self()),
    {ok, Nodes} = file:consult("poller.cf"),
    loop(#state{nodes=Nodes}).


data() ->
    Ref = make_ref(),
    ?MODULE ! {self(), {Ref, get}},
    receive
	{Ref, Data} ->
	    Data
    end.


loop(State) ->
    Res = poll(State#state.nodes),
    NewDict = process(State#state.dict, Res),

    events(NewDict),

    loop(State#state{dict=NewDict}).


poll(Nodes) ->
    Ref = make_ref(),
    poll(Nodes, Ref, []).
poll([], _Ref, Acc) ->
    Acc;
poll([Node|Nodes], Ref, Acc) ->
    {loadavg, Node} ! {self(), {Ref, get}},
    Stuff = receive
		{{Node, Ref}, Result} ->
		    Result
	    after 250 ->
		    {error, timeout}
	    end,
    poll(Nodes, Ref, [{Node, Stuff}|Acc]).


process(Dict, []) ->
    Dict;
process(Dict, [H|T]) ->
    {Node, Result} = H,
    Value = value(Result),
    OldList = case dict:find(Node, Dict) of
		  error ->
		      tuple_to_list(erlang:make_tuple(?MAXDATA-1, undefined));
		  
		  {ok, List} ->
		      case length(List) of
			  ?MAXDATA ->
			      rotate(List);
			  _ ->
			      List
		      end
	      end,
    NewList = [Value|OldList],
    %% FIXME dict:append_list ?
    NewDict = dict:store(Node, NewList, Dict),
    process(NewDict, T).


rotate(D0) ->
    [_|D1] = lists:reverse(D0),
    lists:reverse(D1).


value(undefined) ->
    undefined;
value({error, timeout}) ->
    timeout;
value({ok, #loadavg{?FIELD=L}}) ->
    L.


events(Dict) ->
    receive
	{Pid, {Ref, get}} ->
	    Pid ! {Ref, Dict},
	    events(Dict);
	
	{{_Node, _Ref}, _Res} ->
	    events(Dict);
	
	Other ->
	    io:format("~p got Other: ~p~n", [?MODULE, Other]),
	    events(Dict)
    
    after 1000 ->
	    ok
    end.
