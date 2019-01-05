-module(elev).
-compile(export_all).

init() -> 
    Elevator_PID = spawn(elev, move, [0]),
    put(elev_PID, Elevator_PID),
    % Identifier1_PID = spawn(elev, identify, [1]),
    % put(id1_PID, Identifier1_PID),
    main_loop().


main_loop() ->

    
    timer:sleep(10000),
    %io:format("\n mainloop called \n"),
    NewFloor = rand:uniform(5),
    io:format("\nRAND: ~p",[NewFloor]),
    identify(NewFloor),
    main_loop().

identify(Floor) ->
    %io:format("\n identify called \n"),
    get(elev_PID) ! {success, Floor}.

moveToFloor(CurrentFloor, Floor) when CurrentFloor > Floor ->
    timer:sleep(1000),
    io:format("\nFloor: ~p", [CurrentFloor]),    
    moveToFloor(CurrentFloor-1, Floor);

moveToFloor(CurrentFloor, Floor) when CurrentFloor < Floor ->
    timer:sleep(1000),
    io:format("\nFloor: ~p", [CurrentFloor]),
    moveToFloor(CurrentFloor+1, Floor);

moveToFloor(CurrentFloor, Floor) when CurrentFloor =:= Floor -> 
    io:format("\nFloor arrived: ~p", [CurrentFloor]),
    %get(elev_PID) ! {arrived, CurrentFloor}
    Floor.

%   {arrived, CurrentFloor} ->
%             io:format("arrived"),
%             identify(6),
%             move(CurrentFloor);

move(StartFloor) ->
   %io:format("in move\n"),
    receive
        {success, Floor} -> 
            io:format("Going to floor ~p", [Floor]),
            F = moveToFloor(StartFloor, Floor),
            move(F);    
        {_, Floor} ->
            io:format("Access denied to Floor")
        end.     

