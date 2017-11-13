-module(print).

-export([
         write_final_message/10,
	 write_final_message_bus/7
        ]).


write_final_message( State , Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , LogPID , Mode , csv ) ->

	TotalTime =   CurrentTickOffset - StartTime, 	

	Arrival = io_lib:format( "~w;arrival;~s;~s;~s;~w;~w;~s\n", [ CurrentTickOffset , CarId ,  LastPosition, Mode , TotalTime , TotalLength , Type ] ),

	class_Actor:send_actor_message( LogPID , { receive_action, { Arrival } }, State );






write_final_message( State , Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , LogPID , Mode , xml ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	TotalTime =   CurrentTickOffset - StartTime, 	

	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId ] ),
			
	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
						
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"~s\" trip_time=\"~w\" distance=\"~w\" action=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ,  LastPosition, Mode , TotalTime , TotalLength , Type ] ),

	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , CarId , LastPosition ] ),

	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

	class_Actor:send_actor_message( LogPID , { receive_action, { TextFile } }, State ).




%write_initial_message( State , CurrentTickOffset , LinkOrigin , NewPosition  ) ->


  %	CarId = getAttribute( State , car_name ),
  %	Type = getAttribute( State , type ),

 %  	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  % 	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  %	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
  %	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

%	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),
		
%	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).



%write_movement_car_message( State , CurrentTickOffset , NewPosition ) ->


  	%CarId = getAttribute( State , car_name ),
  	%Type = getAttribute( State , type ),

	%LastPosition = getAttribute( State , car_position ),	

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State ).

%write_movement_bus_message( State , CurrentTickOffset ,  Destination ) ->


%	LastPosition = getAttribute( State , car_position ),
 % 	CarId = getAttribute( State , car_name ),
 % 	Type = getAttribute( State , type ),

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"bus\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"bus\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type ] ),


%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).

%write_movement_metro_message( State , CurrentTickOffset , Destination ) ->

	%CarId = getAttribute( State , car_name ),
  	%Type = getAttribute( State , type ),

	%LastPosition = getAttribute( State , car_position ),

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"metro\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"metro\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type ] ),


%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).





% Bus functions

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

	Arrival = io_lib:format( "~w;arrival;~s;~s;~w\n", [ CurrentTickOffset , BusId ,  LastPosition , TotalTime ] ),

	class_Actor:send_actor_message( LogPID , { receive_action, { Arrival } }, State ).




%write_initial_message( State , CurrentTickOffset , BusId , LinkOrigin , NewPosition ) ->

%	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" />\n", [ CurrentTickOffset , BusId , LinkOrigin ] ),
%   	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , LinkOrigin ] ),
%  	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , BusId ] ),
%  	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , LinkOrigin , BusId ] ),
  					
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [  CurrentTickOffset , BusId , atom_to_list( NewPosition ) , BusId ] ),

%	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),
			
%	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State ).

%write_movement_message( State , CurrentTickOffset , BusId , LastPosition , NewPosition , BusId ) ->

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , atom_to_list(LastPosition) , BusId ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [  CurrentTickOffset , BusId , atom_to_list(NewPosition) , BusId ] ),

%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID , { receive_action, { TextFile } }, State ).


