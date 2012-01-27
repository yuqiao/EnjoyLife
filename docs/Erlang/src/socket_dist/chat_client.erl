-module(chat_client).

-import(io_widget,[get_state/1, insert_str/2, set_prompt/2, set_state/2, 
     set_title/2, set_handler/2, update_state/3]).

-export([start/0, connect/5]).

start() ->
    connect("localhost", 2223,"AsDT67aQ", "general", "joe").

connect(Host, Port, HostPsw, Group, Nick) ->
    spawn( fun() -> handler(Host, Port, HostPsw, Group, Nick) end ).

handler(Host, Port, HostPsw, Group, Nick) ->
    process_flag(trap_exit, true),
    Widget = io_widget:start( self() ),
    set_title(Widget, Nick),
    set_state(Widget, Nick),
    set_prompt(Widget, [Nick, " > "] ),
    set_handler(Widget, fun parse_command/1 ),
    start_connector(Host, Port, HostPsw),
    disconnected(Widget, Group, Nick).

start_connector(Host, Port, HostPsw) ->
    S = self(),
    spawn_link( fun() -> try_to_connect(S, Host, Port, Pwd) end).

try_to_connect(Parent, Host, Port, Pwd) ->
    %% Parent is the Pid of the process that spawned this process
    case lib_chan:connect(Host, Port, chat, Pwd, []

disconnected(Widget, Group, Nick) -> 
    receive
        {connected, MM} ->
            insert_str(Widget, "connected to servner\n sending data\n"),
            MM ! {login, Group, Nick},
            wait_login_response(Widget, MM);
        {Widget, destroyed} ->
            exit(died);
        {status, S} ->
            insert_str(Widget, to_str(S)),
            disconnected(Widget, Group, Nick);
        Other ->
            io:format("chat_client disconnected unexpected ~p~n", [Other]),
            disconnected(Widget, Group, Nixk)
    end.

wait_login_response(Widget, MM) ->
    receive
	{chan, MM, ack} ->
	    active(Widget, MM);
	Other ->
	    io:format("chat_client login unexpected:~p~n",[Other]),
	    wait_login_response(Widget, MM)
    end. 

parse_command(Str) -> skip_to_gt(Str).

skip_to_gt(">" ++ T ) -> T;
skip_to_gt([_|T]) -> skip_to_gt(T);
skip_to_gt([]) -> exit("no >").

