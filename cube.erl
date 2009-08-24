-module(cube).
-author('olivier@biniou.info').

-include_lib("wx/include/gl.hrl").

-export([start/0, loop0/2]).
-export([on/0, off/0]).

-record(state, {enabled=true, gl}).


start() ->
    {Env, GL} = gui:env(),
    Pid = spawn(?MODULE, loop0, [Env, GL]),
    register(?MODULE, Pid),
    gui:connect(Pid).


on() ->
    ?MODULE ! enabled.
off() ->
    ?MODULE ! disabled.


loop0(Env, GL) ->
    wx:set_env(Env),
    State = #state{gl=GL},
    loop(State).

loop(State) ->
    receive
	enabled ->
	    loop(State#state{enabled=true});
	disabled ->
	    loop(State#state{enabled=false});
	{Pid, {Ref, draw}} ->
	    draw(State),
	    Pid ! {ok, Ref},
	    loop(State)
    end.


-define(MAXC, 1.0).
-define(CUBE, {
	  { ?MAXC,  ?MAXC, -?MAXC},   %1
	  { ?MAXC, -?MAXC, -?MAXC},   %2
	  {-?MAXC, -?MAXC, -?MAXC},   
	  {-?MAXC,  ?MAXC, -?MAXC},   %4
	  {-?MAXC,  ?MAXC,  ?MAXC},
	  { ?MAXC,  ?MAXC,  ?MAXC},   %6
	  { ?MAXC, -?MAXC,  ?MAXC}, 
	  {-?MAXC, -?MAXC,  ?MAXC}}). %8


draw(#state{enabled=false}) ->
    ok;
draw(State) ->
    wxGLCanvas:setCurrent(State#state.gl),

    u:set_model_view(),

    gl:lineWidth(2),
    gl:'begin'(?GL_LINES),
    gl:color3ub(255, 255, 255),

    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    gl:vertex3fv(element(4, ?CUBE)),
   
    gl:vertex3fv(element(4, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
   
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(7, ?CUBE)),
    gl:vertex3fv(element(7, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),
    gl:vertex3fv(element(7, ?CUBE)),
    
    gl:vertex3fv(element(6, ?CUBE)),
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(1, ?CUBE)),
    gl:vertex3fv(element(4, ?CUBE)),
    gl:vertex3fv(element(4, ?CUBE)),
    gl:vertex3fv(element(5, ?CUBE)),
    
    gl:vertex3fv(element(7, ?CUBE)),
    gl:vertex3fv(element(2, ?CUBE)),
    gl:vertex3fv(element(3, ?CUBE)),
    gl:vertex3fv(element(8, ?CUBE)),

    gl:'end'().
