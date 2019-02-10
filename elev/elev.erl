-module(elev).
-compile(export_all).

clear_board() -> 
    io:format(os:cmd(clear)).

main() ->
    Main_PID = spawn(elev, finished, []),
    Elevator_PID = spawn(elev, move, [0]),
    Listen_PID = spawn(elev, listen, []),
    Error_Handler_PID = spawn(elev, handle, []),
    Start_Floor_PID = spawn(elev, start_floor, []),
    Halt_PID = spawn(elev, handle_halt, []),
    
    ets:new(pids, [set, named_table]),
    ets:new(var, [set, named_table, public]),
    ets:insert(pids, [{lpid, Listen_PID}, {mpid, Main_PID}, {epid, Elevator_PID}, {error_pid, Error_Handler_PID}, {start_floor_pid, Start_Floor_PID}, {halt_pid, Halt_PID}]),
    [{mpid, Main_PID}] = ets:lookup(pids, mpid),
    clear_board(),
    io:format("\nWelcome to elevator simulator!"),
    timer:sleep(2000),
    Start_Floor_PID ! {start}.

start_floor() ->
    [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
    [{halt_pid, Halt_PID}] = ets:lookup(pids, halt_pid),
    receive
        {start} ->
            clear_board(),
            InputFloors = io:fread("\nEnter requested amount of floors (if floors is lower than 1, the floor selection is random) for your simulation: ","~d"),
            case InputFloors of
                {ok, [Max_Floor]} ->
                    if
                        Max_Floor =< 0 ->
                            ets:insert(var, [{max_floor, 10}, {automatic, -1}]),
                            Halt_PID ! {wait_for_halt},
                            automatic_input();
                        true ->
                            ets:insert(var, [{max_floor, Max_Floor}, {automatic, 0}]),                 
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

handle_halt() -> 
    receive
            {wait_for_halt} ->
                Halt = io:fread("Type anything to quit.", "~s"),
                case Halt of 
                    {ok, _ } ->
                        halt_elev();
                    {error, _} ->
                        halt_elev()
                end
    end.

halt_elev() ->
    clear_board(),
    io:format("Terminating elevator\n"),
    halt().

handle_input() -> 
[{lpid, Listen_PID}] = ets:lookup(pids, lpid),
    [{error_pid, Error_Handler_PID}] = ets:lookup(pids, error_pid),
    [{max_floor, Max_Floor}] = ets:lookup(var, max_floor),    
    InputWhere = io:fread("Enter a floor you are standing on suffixed by spacebar. If you wish to end elevator enter 1q: ","~d~s"),
    
    case InputWhere of
        {ok, [N1, Char1]} -> 
            if Char1 =:= "q" ->
                halt_elev();
            true ->
                InputTo = io:fread("Enter requested floor: ","~d"),    
                case InputTo of
                    {ok, [N2]} ->
                        if  
                            (N1 > Max_Floor) or (N1 < 0) or (N2 > Max_Floor) or (N2 < 0) ->
                                Error_Handler_PID ! {invalid_floor},
                                handle_input();
                            true ->
                                Listen_PID ! {here, N1, N2},
                                handle_input()
                        end;
                   {error, _ } -> 
                        Error_Handler_PID ! {bad_input}
                end
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
            [{automatic, Auto}] = ets:lookup(var, automatic),
            if 
                Auto =/= -1 ->
                    %handle_input(),
                    finished();
                true ->
                    automatic_input(),
                    finished()
            end
    end.

handle() ->
    [{mpid, Main_PID}] = ets:lookup(pids, mpid),
    [{start_floor_pid, Start_Floor_PID}] = ets:lookup(pids, start_floor_pid),
    receive
        {bad_input} ->
            clear_board(), 
            io:format("\nInvalid input\n"),
            handle_input(),
            Main_PID ! {finished},
            handle();
        {bad_input_init} ->
            Start_Floor_PID ! {start},
            handle();
        {invalid_floor} ->
            clear_board(), 
            io:format("\nThere is no floor like that!\n"),
            Main_PID ! {finished},
            handle()     
    end.


moveToFloor(Current_Floor, Floor) when Current_Floor > Floor ->
    [{max_floor, Max_Floor}] = ets:lookup(var, max_floor),
    timer:sleep(1000),
    clear_board(),
    draw_elevator(Current_Floor, Max_Floor),
    moveToFloor(Current_Floor-1, Floor);

moveToFloor(Current_Floor, Floor) when Current_Floor < Floor ->
    [{max_floor, Max_Floor}] = ets:lookup(var, max_floor),
    timer:sleep(1000),
    clear_board(),
    draw_elevator(Current_Floor, Max_Floor),
    moveToFloor(Current_Floor+1, Floor);

moveToFloor(Current_Floor, Floor) when Current_Floor =:= Floor -> 
    [{max_floor, Max_Floor}] = ets:lookup(var, max_floor),
    [{mpid, Main_PID}] = ets:lookup(pids, mpid),
    timer:sleep(1000),
    clear_board(),
    draw_elevator(Current_Floor, Max_Floor),
    Main_PID ! {finished},
    Floor.

% Na 4: 
% uzyc Panel
% proces wyswietla, drugi proces czeka
% obsluge wyjscia w dowolnym momencie
% obsluge q jako wyjscia z programu


move(StartFloor) ->
    receive
        {success, Floor} -> 
            % clear_board(), 
            F = moveToFloor(StartFloor, Floor),
            move(F);
        {success, scan, Floor} ->
            % clear_board(),
            io:format("\nFace scanning"), 
            io:format("\n   oooooooo
  o        o
 o  ^    ^  o 
 o  ?    ?  o
 {     \    }
 |     _|   |
  \\        /
   \\ (--) /
    \\    /
     \\yy/"), 
            io:format("\nFace Scan successful"),
            timer:sleep(1500),
            io:format("\nGoing to floor ~p\n", [Floor]),
            F = moveToFloor(StartFloor, Floor),
            move(F);  
        {failure, scan, Floor} ->
            % clear_board(),
            io:format("\nFace scanning"),
            io:format("\nAccess denied to ~p\n", [Floor]),
            move(StartFloor)
        end.     

draw_elevator(_, Max_Floor) when Max_Floor =:= -1 -> 
    %timer:sleep(100),
    io:format("\n---\n");

draw_elevator(Current_Floor, Max_Floor) when Current_Floor =/= Max_Floor ->
    %timer:sleep(100),
    io:format("\n| | ~p", [Max_Floor]),
    draw_elevator(Current_Floor, Max_Floor - 1);

draw_elevator(Current_Floor, Max_Floor) when Current_Floor =:= Max_Floor ->
    %timer:sleep(100),
    io:format("\n|O| ~p", [Max_Floor]),
    draw_elevator(Current_Floor, Max_Floor - 1).