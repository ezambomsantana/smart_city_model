%Class that represents a simple City Map
-module(class_Street).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , StreetName , ListEdges , LogName , Paths ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Creates a new city graph
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	case ets:info(list_streets) of
		undefined -> ets:new(list_streets, [public, set, named_table]);
                _ -> ok
		end,
		
	case ets:info(list_streets_dr) of
		undefined -> ets:new(list_streets_dr, [public, set, named_table]);
                _ -> ok
        end,

	case ets:info(waiting_bus) of
		undefined -> ets:new(waiting_bus, [public, bag, named_table]);
                _ -> ok
        end,

	iterate_list( ListEdges ),

	create_option_table( LogName , Paths ),

%	InitFile = ets:lookup_element(options, log_file, 2 ),
%	file_utils:write( InitFile, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<events version=\"1.0\">\n" ),

	class_Actor:construct( State, ActorSettings, StreetName ).

create_option_table( LogName , Paths ) ->

	filelib:ensure_dir( LogName ),
	InitFile = file_utils:open( LogName , _Opts=[ write , delayed_write ] ),

	case ets:info(options) of
		undefined -> ets:new(options, [public, set, named_table]);
                _ -> ok
        end,

	ets:insert(options, {log_file, InitFile }),

        code:add_pathsa( Paths ).

%	{ ok, Connection } = amqp_connection:start( #amqp_params_network{} ),
%	{ ok, Channel } = amqp_connection:open_channel( Connection ),

%	Exchange = #'exchange.declare'{ exchange = <<"simulator_exchange">>,
  %                                  type = <<"topic">> },
%	#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

%	Publish = #'basic.publish'{ exchange = <<"simulator_exchange">>,
 %                               routing_key = <<"log_output">> },

%	amqp_channel:cast( Channel,
%					   Publish,
%					   #amqp_msg{ payload = <<"<events version=\"1.0\">\n">> }),


%	ets:insert(options, {rabbitmq_channel, Channel }),
%	ets:insert(options, {rabbitmq_connection, Connection }),
%	ets:insert(options, {rabbitmq_publish, Publish }),
%	ets:insert(options, {rabbitmq_exchange, Exchange }).


iterate_list([]) -> ok;
iterate_list([ Element | List ]) ->
	
	Vertices = element( 1, Element),
	{ Id , Length , _ , Freespeed , Count, Lanes, {} } = element(2, Element),

	% CellSize = 7.5, % Cell size of 7.5m according to MATSim user guide
	CellSize = 7.5,
	CellSizeDR = 4.0,

	case Lanes == 1 of
		true ->
			StorageCapacity = math:ceil((Lanes) * Length / CellSize),
			ets:insert(list_streets, {Vertices,  Id , Length , StorageCapacity , Freespeed , Count, Lanes, {} }),
	
			StorageCapacityDR = math:ceil(1 * Length / CellSizeDR ),
			ets:insert(list_streets_dr, {Vertices,  Id , Length , StorageCapacityDR , Freespeed , Count, Lanes, {} });
		false ->
			StorageCapacity = math:ceil((Lanes - 1) * Length / CellSize),
			ets:insert(list_streets, {Vertices,  Id , Length , StorageCapacity , Freespeed , Count, Lanes, {} }),

			StorageCapacityDR = math:ceil(1 * Length / CellSizeDR ),
			ets:insert(list_streets_dr, {Vertices,  Id , Length , StorageCapacityDR , Freespeed , Count, Lanes, {} })
	end,

	iterate_list( List ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%Connection = ?getAttr(connection),
	%Channel = ?getAttr(channel),
	%Publish = ?getAttr(publish),
	%Exchange = ?getAttr(exchange),

	%#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

	%amqp_channel:cast( Channel,
         %              Publish,
          %             #amqp_msg{ payload = <<"</events>">> } ),

	%ok = amqp_channel:close(Channel),
	%ok = amqp_connection:close(Connection),

%	InitFile = ets:lookup_element(options, log_file, 2 ),
%	file_utils:write( InitFile, "</events>" ),
%	file_utils:close( InitFile ),

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	State.

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).