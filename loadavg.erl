-module(loadavg).
-author('olivier@biniou.info').

-include("loadavg.hrl").

-export([start/0]).


start() ->
    register(?MODULE, self()),
    loop().


loop() ->
    receive
	{Pid, {Ref, get}} ->
	    Load = loadavg(),
	    Message = {{node(), Ref}, Load},
	    Pid ! Message,
	    loop();
	
	stop ->
	    stop;
	
	Other ->
	    io:format("[!] Dropping message: ~p~n", [Other]),
	    loop()
    end.


loadavg() ->
    OS = os:cmd("cat /proc/loadavg"),
    Parsed = io_lib:fread("~f ~f ~f ~d/~d ~d\n", OS),
    {ok, [Load1, Load5, Load15, Runnable, Processes, _LastPID], []} = Parsed,
    Load = #loadavg{load1=Load1,
		    load5=Load5,
		    load15=Load15,
		    runnable=Runnable,
		    processes=Processes},
    {ok, Load}.
    
