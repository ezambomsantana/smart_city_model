-module(class_Log).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, LogName , Paths ).

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
         receive_action/3 ). % publish_data/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Creates a new log actor
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	filelib:ensure_dir( LogName ),
	InitFile = file_utils:open( LogName , _Opts=[ write , delayed_write ] ),

	case ets:info(options) of
		undefined -> ets:new(options, [public, set, named_table]);
                _ -> ok
        end,

	ets:insert(options, {log_pid, self() }),
	ets:insert(options, {log_file, InitFile }),

        code:add_pathsa( Paths ),

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

	ActorState = class_Actor:construct( State, ActorSettings, "log" ),


	setAttributes( ActorState, [
%			{ channel, Channel },
%			{ exchange, Exchange },
%			{ publish, Publish },
%			{ connection, Connection }
		] ).

% The destructor just close the log file.
%
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

%	file_utils:write( InitFile, "</events>" ),
	file_utils:close( ets:lookup_element(options, log_file, 2 ) ),

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	State.

% Receive a message from an agent and saves it in the log file.
-spec receive_action( wooper:state() , car_index() , pid() ) -> wooper:state().
receive_action( State , _Data , _Pid ) ->

%	Channel = ?getAttr(channel),
%	Publish = ?getAttr(publish),

%	Content = element( 1, Data ),

%	amqp_channel:cast( Channel,
 %                      Publish,
  %                     #amqp_msg{ payload = list_to_binary( Content ) }),

	?wooper_return_state_only( State ).

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).


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
