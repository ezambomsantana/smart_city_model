-module(print).

-export([
         write_final_message/8,
	 write_final_message_bus/7,
	 write_initial_message/8,
	 write_movement_car_message/8,
	 write_movement_bus_metro_message/9
        ]).

%%%% CAR MESSAGES %%%%

%%%% ARRIVAL MESSAGE %%%%
write_final_message( _Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , _Mode , csv ) ->

	TotalTime =   CurrentTickOffset - StartTime, 	

%	Arrival = io_lib:format( "~w;arrival;~s;~s;~s;~w;~w;~s\n", [ CurrentTickOffset , CarId ,  LastPosition, Mode , TotalTime , TotalLength , Type ] ),

	Arrival = io_lib:format( "~w;arrival;~s;~s;~w;~w\n", [ CurrentTickOffset , CarId ,  LastPosition, TotalTime , TotalLength ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ), Arrival );


write_final_message( Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , Mode , xml ) ->

	TotalTime =   CurrentTickOffset - StartTime, 	

	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId ] ),
			
	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
						
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"~s\" trip_time=\"~w\" distance=\"~w\" action=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ,  LastPosition, Mode , TotalTime , TotalLength , Type ] ),

	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , CarId , LastPosition ] ),

	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ) , TextFile ).


%%%% START MESSAGE %%%%
write_initial_message( State , LogPID , CarId , _Type , CurrentTickOffset , LinkOrigin , _NewPosition , csv ) ->

	Start = io_lib:format( "~w;start;~s;~s\n", [ CurrentTickOffset , CarId ,  LinkOrigin 	 ] ),

	class_Actor:send_actor_message( LogPID, { receive_action, { Start } }, State );


write_initial_message( State , LogPID , CarId , Type , CurrentTickOffset , LinkOrigin , NewPosition , xml ) ->

  	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
 	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),
		
	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).


%%%% MOVEMENT MESSAGE %%%%
write_movement_car_message( State , CarId , LastPosition , Type , LogPID , CurrentTickOffset , NewPosition , xml ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State );


write_movement_car_message( State , CarId , _LastPosition , _Type , LogPID , CurrentTickOffset , NewPosition , csv ) ->

	Move = io_lib:format( "~w;move;~s;~s\n", [ CurrentTickOffset , CarId ,  atom_to_list( NewPosition ) ] ),

	class_Actor:send_actor_message( LogPID,	{ receive_action, { Move } }, State ).



%%%%%% BUS FUNCTIONS %%%%%%

write_final_message_bus( State , CurrentTickOffset , BusId , LastPosition , StartTime , LogPID , xml ) ->
	
	_TotalTime =   CurrentTickOffset - StartTime, 	

	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , BusId , LastPosition , BusId ] ),
	
	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , BusId , BusId ] ),
				
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , BusId ,  LastPosition ] ),

	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , BusId , LastPosition ] ),

	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),
				
	class_Actor:send_actor_message( LogPID , { receive_action, { TextFile } }, State );

write_final_message_bus( State , CurrentTickOffset , BusId , LastPosition , StartTime , LogPID , csv ) ->

	TotalTime =   CurrentTickOffset - StartTime, 	

	Arrival = io_lib:format( "~w;arrival;~s;~s;~w;0\n", [ CurrentTickOffset , BusId ,  LastPosition , TotalTime ] ),

	class_Actor:send_actor_message( LogPID , { receive_action, { Arrival } }, State ).




write_movement_bus_metro_message( State , CurrentTickOffset , _LastPosition , CarId , _Type , Destination , TripType , LogPID , csv ) ->

	Move = case TripType of		
		bus ->
			io_lib:format( "~w;move_bus;~s;~s\n", [ CurrentTickOffset , CarId , Destination ] );
		metro ->
			io_lib:format( "~w;move_metro;~s;~s\n", [ CurrentTickOffset , CarId , Destination ] )
	end,

	class_Actor:send_actor_message( LogPID, { receive_action, { Move } }, State );

write_movement_bus_metro_message( State , CurrentTickOffset , LastPosition , CarId , Type , Destination , TripType , LogPID , xml ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"~s\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type , atom_to_list( TripType ) ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"~s\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type , atom_to_list( TripType ) ] ),

	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).
