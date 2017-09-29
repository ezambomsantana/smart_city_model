%Class that manage the parking spots in the city
-module(class_Parking).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , SpotName , ListOfSpots, LogPID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2,
         spot_available/3, spot_in_use/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Creates a list with the parking spots in the city
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter(), pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

    AvailableParkingSpots = dict:from_list( ListOfSpots ),
    UnavailableParkingSpots = dict:new(),

    %print( ListOfSpots ),

    ActorState = class_Actor:construct( State, ActorSettings , SpotName ),

    setAttributes( ActorState, [
                                { logPID, LogPID },
                                { availableSpots , AvailableParkingSpots },
                                { unavailableSpots , UnavailableParkingSpots } ] ).


%print( [] ) ->
%    ok;

%print( [ Obj | List ] ) ->

%    Uuid = element( 1 , Obj ),
%    IdNo = element( 2 , Obj ),
%    io:format("spot: ~s ~s" , [ Uuid , IdNo ] ),
%    print( List ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

    AvailableParkingSpots = getAttribute( State, availableSpots ),
    UnavailableParkingSpots = getAttribute( State, unavailableSpots ),

    CurrentTick = class_Actor:get_current_tick_offset( State ),

    FreeSpots = fun( SpotUUID, { NodeGraphId, Tick }) ->
                    case ( ( Tick - CurrentTick ) < 1200 ) of
                        true -> true;
                        false ->
                            setAttribute( State, availableSpots, dict:append( SpotUUID, NodeGraphId, AvailableParkingSpots ) ),
                            false
                    end
                end,

    setAttribute( State, unavailableSpots, dict:filter( FreeSpots, UnavailableParkingSpots )),

    executeOneway( State , addSpontaneousTick, CurrentTick + 600 ),

	State.

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	?wooper_return_state_only( ScheduledState ).


-spec spot_available( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
spot_available( State , SpotUUID , PersonPID ) ->
	
    AvailableParkingSpots = getAttribute( State, availableSpots ),

    case dict:find( element( 1 , SpotUUID ) , AvailableParkingSpots ) of
        { ok, GraphNodeID } ->
            class_Actor:send_actor_message( PersonPID, { get_parking_spot, { element( 1 , GraphNodeID ) } }, State );
        error ->
            class_Actor:send_actor_message( PersonPID, { get_parking_spot, { nok } }, State )
    end.


-spec spot_in_use( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
spot_in_use( State, SpotUUID, _PersonID ) ->

	LogPID = getAttribute( State, logPID ),
	AvailableParkingSpots = getAttribute( State, availableSpots ),
	UnavailableParkingSpots = getAttribute( State, unavailableSpots ),


	CurrentTick = class_Actor:get_current_tick_offset( State ),

	UUID = element( 1 , SpotUUID ),

	GraphNodeID = dict:fetch( UUID , AvailableParkingSpots ),
	NewState = setAttribute( State, availableSpots, dict:erase( UUID, AvailableParkingSpots ) ),
	NewNewState = setAttribute( NewState , unavailableSpots, dict:append( UUID, { GraphNodeID, CurrentTick }, UnavailableParkingSpots ) ),

	change_spot_state( NewNewState , UUID, false, LogPID ).


change_spot_state( State , SpotUUID, Available, LogPID ) ->

    Topic = "data_stream",
    RoutingKey = string:concat( SpotUUID, ".parking_monitoring.simulated" ),

    { { Year, Month, Day }, { Hour, Minute, Second } } = calendar:local_time(),
    Timestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                                          [ Year, Month, Day, Hour, Minute, Second ] ) ),

    SpotState = lists:flatten( io_lib:format( "~p", [ Available ] ) ),

    Message = "{\"parking_monitoring\": [
                  {\"available\": \"" ++ SpotState ++ "\"," ++
                   "\"timestamp\": \"" ++ Timestamp ++ "\"}]}",

    Data = { Topic, RoutingKey, Message },
    class_Actor:send_actor_message( LogPID, { publish_data, { Data } }, State ).

