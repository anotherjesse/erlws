-module(local_server).
-compile(export_all).


start() ->
    Pid = spawn(fun() -> manage_clients([]) end),
    register(client_manager, Pid),
    {ok, Listen} = gen_tcp:listen(1234, [{packet,0},
                                         {reuseaddr,true},
                                         {active, true}]),
    spawn(fun() -> par_connect(Listen) end).


par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    client_manager ! {connect, Socket},
    wait(Socket).


wait(Socket) ->
    receive
        {tcp, Socket, _} ->
            Msg = prefix() ++
                "WebSocket-Origin: http://localhost\r\n" ++
                "WebSocket-Location: ws://localhost:1234/\r\n\r\n",
            gen_tcp:send(Socket, Msg),
            loop(Socket);
        Any ->
            io:format("WAIT FIXME: ~p~n",[Any]),
            wait(Socket)
    end.


prefix() ->
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n".


loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            Data1 = unframe(Data),
            io:format("data: ~p~n",[Data1]),
            client_manager ! {data, Socket, Data1},
            loop(Socket);
        {tcp_closed, Socket} ->
            client_manager ! {disconnect, Socket};
        Any ->
            io:format("LOOP FIXME: ~p~n",[Any]),
            loop(Socket)
    end.


unframe([0|T]) -> unframe1(T).
unframe1([255]) -> [];
unframe1([H|T]) -> [H|unframe1(T)].


manage_clients(Sockets) ->
    receive
        {connect, Socket} ->
            io:format("Socket connected: ~w~n", [Socket]),
            NewSockets = [Socket | Sockets];
        {disconnect, Socket} ->
            io:format("Socket disconnected: ~w~n", [Socket]),
            NewSockets = lists:delete(Socket, Sockets);
        {data, User, Data} ->
            send_data(Sockets, User, Data),
            NewSockets = Sockets
    end,
    manage_clients(NewSockets).


send_data(Sockets, User, Data) ->
    SendData = fun(Socket) ->
                       gen_tcp:send(Socket, [0] ++ io_lib:format("~p", [User]) ++ ": " ++ Data ++ [255])
               end,
    lists:foreach(SendData, Sockets).
