-module(elev).
-compile(export_all).

main() ->
    Main_PID = spawn(elev, finished, []),
    Elevator_PID = spawn(elev, move, [0]),
    Listen_PID = spawn(elev, listen, []),
    Error_Handler_PID = spawn(elev, handle, []),
    Start_Floor_PID = spawn(elev, start_floor, []),

    % mozna dodac tryb losowy
    % proces inicjalizujacy (zbiera informacje o windzie na poczatku)
    % proces konczacy cala zabawe
    
    ets:new(pids, [set, named_table]),
    ets:new(var, [set, named_table, public]),
    ets:insert(pids, [{lpid, Listen_PID}, {mpid, Main_PID}, {epid, Elevator_PID}, {error_pid, Error_Handler_PID}, {start_floor_pid, Start_Floor_PID}]),
    [{mpid, Main_PID}] = ets:lookup(pids, mpid),

    io:format("Welcome to elevator simulator!"),
    timer:sleep(1000),
    Start_Floor_PID ! {start}.

%%odpal start_floor       

start_floor() ->
    [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
    receive
        {start} ->
            io:format(os:cmd(clear)),
            InputFloors = io:fread("Enter requested amount of floors for your simulation: ","~d"),
            case InputFloors of
                {ok, [Max_Floor]} ->
                    if
                        Max_Floor =< 0 ->
                            ets:insert(var, [{max_floor, -1}]),
                            automatic_input();
                        true ->
                            ets:insert(var, [{max_floor, Max_Floor}]),                 
                            handle_input()
                    end;
                {error, _} -> 
                    Error_Handler_PID ! {bad_input_init},
                    start_floor()
            end
    end.

automatic_input()->
    [{lpid, Listen_PID}] = ets:lookup(pids, lpid),
    N1 = rand:uniform(10),
    N2 = rand:uniform(10),
    Listen_PID ! {here, N1, N2}.

handle_input() -> 
    InputWhere = io:fread("\nEnter a floor you are standing on: ","~d"),
    InputTo = io:fread("\nEnter requested floor: ","~d"),
    [{lpid, Listen_PID}] = ets:lookup(pids, lpid),
    [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
    [{max_floor, Max_Floor}] = ets:lookup(var, max_floor),
    case InputWhere of
        {ok, [N1]} -> case InputTo of
            {ok, [N2]} ->
                io:format("\nFrom: ~p | To: ~p \n",[N1, N2]),
                if 
                    (N1 > Max_Floor) or (N1 < 0) or (N2 > Max_Floor) or (N2 < 0) ->
                        Error_Handler_PID ! {invalid_floor};
                    true ->
                        Listen_PID ! {here, N1, N2}
                end;
            {error, _ } -> 
                Error_Handler_PID ! {bad_input}
        end;
        {error, _ } -> 
            Error_Handler_PID ! {bad_input}
    end.
    

listen() ->
    receive
        {here, Where, To} -> 
            identify(Where, To),
            listen()
    end.        

identify(CurrentFloor, RequestedFloor) ->
    %io:format("\n identify called \n"),
    [{epid, Elev_PID}] = ets:lookup(pids, epid),
    Elev_PID ! {success, CurrentFloor},
    Random = rand:uniform(10),
    if 
        Random >= 3  -> Elev_PID ! {success, scan, RequestedFloor};
        Random < 3 -> Elev_PID ! {failure, scan, RequestedFloor}
    end.

finished() -> 
    receive 
        {finished} ->
            [{max_floor, Max_Floor}] = ets:lookup(var, max_floor),
            if 
                Max_Floor =/= -1 ->
                    handle_input(),
                    finished();
                true ->
                    io:format("true"),
                    automatic_input(),
                    finished()
            end
    end.

handle() ->
    [{mpid, Main_PID}] = ets:lookup(pids, mpid),
    [{start_floor_pid, Start_Floor_PID}] = ets:lookup(pids, start_floor_pid),
    receive
        {bad_input} ->
            io:format(os:cmd(clear)), 
            io:format("\nInvalid input"),
            Main_PID ! {finished},
            handle();
        {bad_input_init} ->
            Start_Floor_PID ! {start},
            handle();
        {invalid_floor} ->
            io:format(os:cmd(clear)), 
            io:format("\nThere is no floor like that!"),
            Main_PID ! {finished},
            handle()                                                
    end.


moveToFloor(CurrentFloor, Floor) when CurrentFloor > Floor ->
    timer:sleep(1000),
    io:format("\nFloor: ~p", [CurrentFloor]),    
    moveToFloor(CurrentFloor-1, Floor);

moveToFloor(CurrentFloor, Floor) when CurrentFloor < Floor ->
    timer:sleep(1000),
    io:format("\nFloor: ~p", [CurrentFloor]),
    moveToFloor(CurrentFloor+1, Floor);

moveToFloor(CurrentFloor, Floor) when CurrentFloor =:= Floor -> 
    io:format("\nFloor arrived: ~p\n", [CurrentFloor]),
    [{mpid, Main_PID}] = ets:lookup(pids, mpid),    
    Main_PID ! {finished},
    Floor.

move(StartFloor) ->
   %io:format("in move\n"),
    receive
        {success, Floor} -> 
            io:format("\nGoing to floor ~p", [Floor]),
            F = moveToFloor(StartFloor, Floor),
            move(F);
        {success, scan, Floor} ->
            io:format("\nFace scanning"), 
            io:format("\nFace Scan successful"),
            io:format("\nGoing to floor ~p", [Floor]),
            F = moveToFloor(StartFloor, Floor),
            move(F);  
        {failure, scan, Floor} ->
            io:format("\nFace scanning"),
            io:format("\nAccess denied to ~p", [Floor]),
            move(StartFloor)
        end.     

