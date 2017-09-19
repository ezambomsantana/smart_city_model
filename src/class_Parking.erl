%Class that manage the parking spots in the city
-module(class_Parking).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , ListOfSpots ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).

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
				class_Actor:name() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

    % Formatter transforms the { UUID, ID } to { UUID, { ID, availability } }
    Formatter = fun(Spot) -> { element( 1, Spot ), { element( 2, Spot ), true } } end,
    ParkingSpotsAvailable = lists:map( Formatter, ListOfSpots ),
    ParkingSpots = dict:from_list( ParkingSpotsAvailable ),

    { ok, Connection } = amqp_connection:start( #amqp_params_network{} ),
    { ok, Channel } = amqp_connection:open_channel( Connection ),

    Exchange = #'exchange.declare'{ exchange = <<"data_stream">>,
                                    type = <<"topic">> },
    #'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

    ActorState = class_Actor:construct( State, ActorSettings ),

    setAttributes( ActorState, [
                                { channel, Channel },
                                { connection, Connection }
                                { spots , ParkingSpots } ] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    Channel = getAttribute( State, channel ),
    Connection = getAttribute( State, connection ),

    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),

    State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	State.


-spec spot_available( wooper:state(), parameter(), pid() ) ->
					   class_Actor:actor_oneway_return().
spot_available( State , SpotUUID , PersonPID ) ->
	
    ParkingSpots = getAttribute( State, spots ),

    SpotData = dict:fetch( SpotUUID, ParkingSpots ),
    GraphNodeID = element( 1, SpotData ),
    Available = element( 2, SpotData ),

    case Available of
        true ->
            class_Actor:send_actor_message( PersonPID, { get_parking_spot, { GraphNodeID } }, State ).
        false ->
            class_Actor:send_actor_message( PersonPID, { get_parking_spot, { nok } }, State ).


-spec spot_in_use( wooper:state(), parameter(), pid() ) ->
					   class_Actor:actor_oneway_return().
spot_in_use( State, SpotUUID, PersonID ) ->
	
    ParkingSpots = getAttribute( State, spots ),
    Channel = getAttribute( State, channel ),

    StateUpdater = fun( Spot ) -> { element( 1, Spot ), false } end,
    setAttribute( State, spots, dict:update( SpotUUID, StateUpdater, ParkingSpots ).

    publish_data( Channel, SpotUUID, false ).


publish_data( Channel, SpotUUID, Available ) ->

    RoutingKey = string:concat( SpotUUID, ".parking_monitoring.simulated" ),
    Publish = #'basic.publish'{ exchange = <<"data_stream">>,
                                routing_key = list_to_binary( RoutingKey ) },

    { { Year, Month, Day }, { Hour, Minute, Second } } = calendar:local_time(),
    Timestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                                              [ Year, Month, Day, Hour, Minute, Second ] ) ),

    State = lists:flatten( io_lib:format( "~p", [ Available ] ),

    Message = "{\"parking_monitoring\": [
                    {\"available\": \"" ++ State ++ "\",
                    "\"timestamp\": \"" ++ Timestamp ++ "\"
                    }
               ]}",

    amqp_channel:cast( Channel,
                       Publish,
                       #amqp_msg{ payload = list_to_binary( Message ) }),

    ok.
