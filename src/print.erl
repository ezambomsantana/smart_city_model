-module(print).

-export([
         write_final_message/8,
	 write_final_message_bus/5,
	 write_initial_message/6,
	 write_movement_car_message/6,
	 write_movement_bus_metro_message/7,
	 write_sensor_data/3
        ]).

%%%% CAR MESSAGES %%%%

%%%% ARRIVAL MESSAGE %%%%
write_final_message( _Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , _Mode , csv ) ->

	{ Hour , Minute } = get_hour_minute(),

	TotalTime =   CurrentTickOffset - StartTime, 	

%	Arrival = io_lib:format( "~w;arrival;~s;~s;~w;~w;~w;~s\n", [ CurrentTickOffset , CarId ,  LastPosition, Mode , TotalTime , TotalLength , Type ] ),

	Arrival = io_lib:format( "~w;~w;~w;arrival;~s;~s;~w;~w\n", [ Hour , Minute , CurrentTickOffset , CarId ,  LastPosition, TotalTime , TotalLength ] ),

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
write_initial_message( CarId , _Type , CurrentTickOffset , LinkOrigin , _NewPosition , csv ) ->

	{ Hour , Minute } = get_hour_minute(),

	Start = io_lib:format( "~w:~w;~w;start;~s;~s\n", [ Hour , Minute , CurrentTickOffset , CarId ,  LinkOrigin 	 ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ), Start );

write_initial_message( CarId , Type , CurrentTickOffset , LinkOrigin , NewPosition , xml ) ->

  	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
 	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),
		
	file_utils:write( ets:lookup_element(options, log_file, 2 ), TextFile ).


%%%% MOVEMENT MESSAGE %%%%
write_movement_car_message( CarId , LastPosition , Type , CurrentTickOffset , NewPosition , xml ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ), TextFile );


write_movement_car_message( CarId , _LastPosition , _Type , CurrentTickOffset , NewPosition , csv ) ->


	{ Hour , Minute } = get_hour_minute(),
	Move = io_lib:format( "~w:~w;~w;move;~s;~s\n", [ Hour , Minute , CurrentTickOffset , CarId ,  atom_to_list( NewPosition ) ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ), Move ).



%%%%%% BUS FUNCTIONS %%%%%%

write_final_message_bus( CurrentTickOffset , BusId , LastPosition , _StartTime , xml ) ->

	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , BusId , LastPosition , BusId ] ),
	
	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , BusId , BusId ] ),
				
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , BusId ,  LastPosition ] ),

	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , BusId , LastPosition ] ),

	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),
				
	file_utils:write( ets:lookup_element(options, log_file, 2 ), TextFile );

write_final_message_bus( CurrentTickOffset , BusId , LastPosition , StartTime , csv ) ->
	
	{ Hour , Minute } = get_hour_minute(),
	TotalTime =   CurrentTickOffset - StartTime, 	

	Arrival = io_lib:format( "~w:~w;~w;arrival;~s;~s;~w;0\n", [ Hour , Minute , CurrentTickOffset , BusId ,  LastPosition , TotalTime ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ), Arrival ).




write_movement_bus_metro_message( CurrentTickOffset , _LastPosition , CarId , _Type , Destination , TripType , csv ) ->
	
	{ Hour , Minute } = get_hour_minute(),
	Move = case TripType of		
		bus ->
			io_lib:format( "~w:~w;~w;move_bus;~s;~s\n", [ Hour , Minute , CurrentTickOffset , CarId , Destination ] );
		metro ->
			io_lib:format( "~w:~w~w;move_metro;~s;~s\n", [ Hour , Minute , CurrentTickOffset , CarId , Destination ] )
	end,
	file_utils:write( ets:lookup_element(options, log_file, 2 ), Move );

write_movement_bus_metro_message( CurrentTickOffset , LastPosition , CarId , Type , Destination , TripType , xml ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"~s\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type , atom_to_list( TripType ) ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"~s\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type , atom_to_list( TripType ) ] ),

	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	file_utils:write( ets:lookup_element(options, log_file, 2 ), TextFile ).


write_sensor_data( Name , Type , Value ) ->
	{ Hour , Minute } = get_hour_minute(),
	Data = io_lib:format( "~w:~w;~s;sensor_data;~s;~s\n", [ Hour , Minute , Name , Type , Value ] ),
	file_utils:write( ets:lookup_element(options, log_file, 2 ), Data ).


get_hour_minute() ->
	TS = os:timestamp(),
	{{_,_,_},{Hour,Minute,_}} = calendar:now_to_universal_time(TS),
	{ Hour , Minute }.

% Receive a message from an agent and saves it in the log file.
%-spec publish_data( wooper:state() , parameter() , pid() ) -> wooper:state().
%publish_data( State , Data , _Pid ) ->

%	Channel = ?getAttr(channel),

%	Topic = element ( 1, Data ),
%	RoutingKey = element( 2, Data ),
%	Message = element( 3, Data ),

%	Exchange = #'exchange.declare'{ exchange = list_to_binary( Topic ),
%									type = <<"topic">> },
%	#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

%	Publish = #'basic.publish'{ exchange = list_to_binary( Topic ),
%								routing_key = list_to_binary( RoutingKey ) },

%	amqp_channel:cast( Channel,
%					   Publish,
%					   #amqp_msg{ payload = list_to_binary( Message ) }).
