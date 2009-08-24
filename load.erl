-module(load).
-author('olivier@biniou.info').

-include_lib("wx/include/gl.hrl").
-include("ss.hrl").

-export([start/0, loop0/2]).
-export([on/0, off/0]).

-define(POLLER,   'poller@yoda').
-define(COLORMAP, "VOLCANO2.map.bin").
-define(MAX,      4.0).

-define(BLACK,    {0, 0, 0}).
-define(V,        127).
-define(WHITE,    {?V, ?V, ?V}). %% a grey, in fact
-define(RED,      {255, 0, 0}).

-define(FOV, 80).

-record(state, {enabled=true, gl, cmap, last, list}).


start() ->
    {Env, GL} = gui:env(),
    Pid = spawn(?MODULE, loop0, [Env, GL]),
    register(?MODULE, Pid),
    gui:connect(Pid).


on() ->
    ?MODULE ! enabled.
off() ->
    ?MODULE ! disabled.


fix(C0) ->
    C1 = setelement(1, C0, ?WHITE),
    setelement(256, C1, ?RED).


loop0(Env, GL) ->
    wx:set_env(Env),
    Cmap0 = cmap:new(?COLORMAP),
    Cmap1 = fix(Cmap0),
    State = #state{gl=GL, cmap=Cmap1},
    loop(State).

loop(State) ->
    receive
	enabled ->
	    loop(State#state{enabled=true});
	disabled ->
	    loop(State#state{enabled=false});
	{Pid, {Ref, draw}} ->
	    UTS = u:uts(),
	    List = gl(State, UTS),
	    Pid ! {ok, Ref},
	    loop(State#state{last=UTS, list=List})
    end.


gl(#state{enabled=false, list=List}, _) ->
    List;
gl(State, UTS) ->
    wxGLCanvas:setCurrent(State#state.gl),

    List = State#state.list,
    L = if
	    UTS == State#state.last ->
		gl:callList(List),
		List;
	    true ->
		delete_list(List),
		make_list(State#state.cmap)
	end,
    L.


delete_list(undefined) ->
    ok;
delete_list(List) when is_integer(List) ->
    gl:deleteLists(List, 1).


make_list(Cmap) ->
    List = gl:genLists(1),

    Data0 = data(),
    Data1 = preprocess(Data0, Cmap),

    gl:newList(List, ?GL_COMPILE_AND_EXECUTE),
    u:set_model_view(),

    draw(Data1),

    gl:endList(),
    List.


data() ->
    Data0 = rpc:call(?POLLER, poller, data, []),
    case Data0 of
	{badrpc, nodedown} ->
	    io:format("Poller node not running !~n", []),
	    undefined;
	Dict ->
	    dict:to_list(Dict)
    end.


draw(undefined) ->
    ok;
draw(Data) ->
    Nodes = length(Data),
    gl:translatef(-1.0, -1.0, -1.0),
    gl:scalef(2.0/Nodes, 0.5, 2.0/(?MAXDATA-1)),
    draw(Data, ?ZERO).


draw([], _X) ->
    ok;
draw([Node|Nodes], X) ->
    draw_node(Node, X),
    draw(Nodes, X+?ONE).

draw_node({_Node, Values}, X) ->
    draw_node(lists:reverse(Values), X, ?ZERO).
draw_node([_Last], _X, _Z) ->
    ok;
draw_node([{Y0, Bot0, Top0}, {Y1, Bot1, Top1} = Second | T], X, Z) ->
    Edges = {
      {{X,      ?ZERO, Z},      Bot0}, %% E1
      {{X+?ONE, ?ZERO, Z},      Bot0}, %% E2
      {{X+?ONE, ?ZERO, Z+?ONE}, Bot1}, %% E3
      {{X,      ?ZERO, Z+?ONE}, Bot1}, %% E4
      {{X,      Y0,    Z},      Top0}, %% E5
      {{X+?ONE, Y0,    Z},      Top0}, %% E6
      {{X+?ONE, Y1,    Z+?ONE}, Top1}, %% E7
      {{X,      Y1,    Z+?ONE}, Top1}  %% E8
     },
    draw_edges(Edges),
    draw_node([Second|T], X, Z+?ONE).


-define(Edge(N),    element(N, Edges)).
-define(EdgePos(N), element(1, ?Edge(N))).
-define(EdgeCol(N), element(2, ?Edge(N))).

-define(E(N),       e(N, Edges)).

e(N, Edges) ->
    Pos = ?EdgePos(N),
    Col = ?EdgeCol(N),
    gl:color3ubv(Col),
    gl:vertex3fv(Pos).


draw_edges(Edges) ->
    gl:'begin'(?GL_QUAD_STRIP),
    ?E(3), ?E(4),
    ?E(2), ?E(1),
    ?E(6), ?E(5),
    ?E(7), ?E(8),
    ?E(3), ?E(4),
    gl:'end'(),
    gl:'begin'(?GL_QUADS),
    ?E(1), ?E(5), ?E(8), ?E(4),
    ?E(2), ?E(6), ?E(7), ?E(3),
    gl:'end'().


preprocess(undefined, _Colormap) ->
    undefined;
preprocess(Data, Colormap) ->
    preprocess(Data, Colormap, []).
preprocess([], _Colormap, Acc) ->
    Acc;
preprocess([H|T], Colormap, Acc) ->
    {Node, Values} = H,
    NewValues = values(Values, Colormap),
    Res = {Node, NewValues},
    preprocess(T, Colormap, [Res|Acc]).


values(Values, Colormap) ->
    values(Values, Colormap, []).
values([], _Colormap, Acc) ->
    lists:reverse(Acc);
values([V|Vs], Colormap, Acc) ->
    Clamped = clamp(V),
    Bot = bot(V),
    Top = top(V, Colormap),
    Res = {Clamped, Bot, Top},
    values(Vs, Colormap, [Res|Acc]).


clamp(undefined) ->
    0.0;
clamp(timeout) ->    
    0.0;
clamp(Val) when Val > ?MAX ->
    ?MAX;
clamp(Val) when Val >= 0.0 -> %% a negative load will be a Problem
    Val.


bot(timeout) ->
    ?RED;
bot(_) ->
    ?WHITE.


top(undefined, _Colormap) ->
    ?WHITE;
top(timeout, _Colormap) ->
    ?RED;
%% V MUST be in range 0.0 .. +?MAX or we crash !
top(V, Colormap) when V >= 0.0, V =< ?MAX->
    Idx1 = V /?MAX,         %% [0..MAX => 0..1]
    Idx2 = Idx1 * 255.0,    %% [0..255]
    Idx3 = trunc(Idx2 + 1), %% [1..256] (integer)
    element(Idx3, Colormap).


%% =============================================================================
%% == Cube definition
%% =============================================================================

%% Edges: (E1 .. E8)
%% Data:  [V0, V1, ...] values in [0.0 .. ?MAX]

%%                 _________________________
%%              E8/ _____________________  /E7 <- Y1 = f(V1)
%%               / / ___________________/ / |
%%              / / /| |               / /  |
%%             / / / | |              / / . |
%%            / / /| | |             / / /| |
%%           / / / | | |            / / / | |
%%          / / /  | | |           / / /| | |
%%         / /_/__________________/ / / | | |
%%      E5/________________________/E6 <- Y0 = f(V0)
%%        | ______________________ | |  | | |
%%        | | |    | | |_________| | |__| | |
%%        | | |    | |___________| | |____| |
%%        | | | E4/ / ___________| | |_  / /E3
%%        | | |  / / /           | | |/ / /
%%        | | | / / /            | | | / /                       ^ Y+
%%        | | |/ / /             | | |/ /                        |
%%        | | | / /              | | ' /                         |
%%        | | |/_/_______________| |  /                          |   
%%        | |____________________| | /                           |
%%      E1|________________________|/E2                          o-----> X+
%%                                                              /
%%                                                             /
%%                                                            /
%%                                                           v Z+
%%
%% E1 = (X  ,  0, Z  )
%% E2 = (X+1,  0, Z  )
%% E3 = (X+1,  0, Z+1)
%% E4 = (X,    0, Z+1)
%% E5 = (X,   Y0, Z  )
%% E6 = (X+1, Y0, Z  )
%% E7 = (X+1, Y1, Z+1)
%% E8 = (X,   Y1, Z+1)
%%
