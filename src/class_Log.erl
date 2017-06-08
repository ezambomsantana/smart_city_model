-module(class_Log).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, LogName ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, receive_action/3).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Creates a new log actor
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, LogName ),

	InitFile = file_utils:open( LogName , _Opts=[ append, delayed_write ] ),

	file_utils:write( InitFile, "<events version=\"1.0\">\n" ),


	setAttributes( ActorState, [
		{ file , InitFile } ] ).

% The destructor just close the log file.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	InitFile = ?getAttr(file),

	file_utils:write( InitFile, "</events>" ),

	file_utils:close( InitFile ),

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	State.

% Receive a message from an agent and saves it in the log file.
-spec receive_action( wooper:state() , car_index() , pid() ) -> wooper:state().
receive_action( State , Data , _Pid ) ->

	InitFile = ?getAttr(file),

	file_utils:write( InitFile, element( 1 , Data ) ),

	?wooper_return_state_only( State ).

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).


