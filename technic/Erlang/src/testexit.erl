-module(testexit).

-export([start/0]).

start() ->
    io:format("Hello, ~p~n", ["yuqiao"] ),
    process_flag(trap_exit, true),
    S = self(),
    spawn_link(fun() -> tell_father(S) end),
    loop().

tell_father(Father) ->
    sleep(500),
    Father ! { ok, "Daddy" },
    sleep(2000),
    exit( connectorFinished ).

loop() ->
    receive
        {ok, Msg} -> 
            io:format("message from child: ~p~n", [Msg] ),
            loop();
        Other ->
            io:format("Wrong Msg: ~p~n", [Other] ),
            loop()
    after 1000 ->
        io:format("Timeout, no message!"),
        loop()
    end.

sleep(T) ->
    receive
    after T -> true
    end.


