-module(class_Bike).
% based on class_Car

% "Teste" de class_Bike é a compilação.
%
% Método 1
% Primeiro roda "erlc class_Bike.erl" pra antecipar alguns erros de sintaxe.
% Aí roda "docker build -t interscsimulator .", que vai tentar compilar class_Bike
% e dar erro se não conseguir.
%
% Método 2
% Instalar o Erlang 20 (https://github.com/kerl/kerl).
% Aí roda "make all" na raiz do zip só uma vez, para compilar o sim-diasca.
% Depois roda "make" em zip/mock-simulators/smart_city_model/src,
% isso vai compilar todas as classes em src, inclusive class_bike.
% Esse método dá mais trabalho pra configurar, mas a compilação fica instântanea.
% No método 1 demora bastante pra ter feedback.

% remover
% DigitalRailsCapable

-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
% Trips: list of trips since the trip can be a multi-trip
%        each trip is a tuple { Mode , Path , LinkOrigin }
%        LinkOrigin is not used here; Mode is also not used, since here we assume mode is always bike 
% Type: reason for the trip; not used
% Mode: bike
% Park and DigitalRailsCapable: not used (it seems it's hard to remove them from the constructor without ofending the compiler)
-define( wooper_construct_parameters, ActorSettings, BikeName , Trips , StartTime , Type , _Park , Mode, _DigitalRailsCapable ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/8, new_link/8,
		 synchronous_new/8, synchronous_new_link/8,
		 synchronous_timed_new/8, synchronous_timed_new_link/8,
		 remote_new/9, remote_new_link/9, remote_synchronous_new/9,
		 remote_synchronous_new_link/9, remote_synchronisable_new_link/9,
		 remote_synchronous_timed_new/9, remote_synchronous_timed_new_link/9,
		 construct/9, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, receive_signal_state/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, BikeName ),

	InitialTrip = lists:nth( 1 , Trips ),	
	Path = element( 2 , InitialTrip ),

	case Mode of 
		bike -> ok;
		_ ->
			erlang:error("It should never happen; class_Bike should receive only bike as mode, but it received [" ++ Mode ++ "].")
	end,

	% each bike agent has a different personal speed
	PersonalSpeed = traffic_models:get_personal_bike_speed(),

	InitialState = setAttributes( ActorState, [
		{ bike_name, BikeName },
		{ trips , Trips },
		{ type, Type },
		{ distance , 0 },
		{ bike_position, -1 },
		{ start_time , StartTime },
		{ path , Path },
		{ mode , Mode },
		{ last_vertex , ok },
		{ last_vertex_pid , ok },
		{ previous_dr_name, nil },
        { personal_speed, PersonalSpeed}]
	),
    InitialState.


-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
	Trips = getAttribute( State , trips ), 
	Path = getAttribute( State , path ), 
	verify_next_action( State , Trips , Path ).

verify_next_action( State , _Trip , Path ) when Path == false ->
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path ) when length( Trips ) == 0, Path == finish -> 
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path ) when length( Trips ) > 0 ->
	?wooper_return_state_only( request_position( State , Path ) );

verify_next_action( State , _Trips , _Path ) ->
	Type = getAttribute( State , type ),						
	TotalLength = getAttribute( State , distance ),
	StartTime = getAttribute( State , start_time ),
	BikeId = getAttribute( State , bike_name ),	
	LastPosition = getAttribute( State , bike_position ),
	Mode = getAttribute( State , mode ), 

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	print:write_final_message( Type , TotalLength , StartTime , BikeId , CurrentTickOffset , LastPosition , Mode , csv ),
	PathFinish = setAttribute( State , path , finish ),

	executeOneway( PathFinish , scheduleNextSpontaneousTick ).

request_position( State , Path ) when Path == finish ->
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
	Trips = getAttribute( State , trips ), 
	NewTrips = list_utils:remove_element_at( Trips , 1 ),
	
	NewState = case length( NewTrips ) > 0 of
		true -> 
			InitialTrip = lists:nth( 1 , NewTrips ),	
			NewPath = element( 2 , InitialTrip ),
			setAttributes( State , [ { trips , NewTrips } , { path, NewPath} ] );
		false -> 
			setAttributes( State , [ { trips , NewTrips } , { path, ok} ] )
	end,
	executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	


request_position( State , Path ) ->
	case length( Path ) > 1 of
		true ->	get_next_vertex( State , Path );
		false -> 
	        FinalState = setAttribute( State, path , finish ),
	        executeOneway( FinalState , scheduleNextSpontaneousTick )
	end.



get_next_vertex( State, [ CurrentVertex | _ ] ) -> % Baseado no Mode != walk do class_car
	LastVertex = getAttribute(State, last_vertex),

	% Current vertex is an atom here, but at the ets it is a string. Must convert:
	CurrentVertexStr = lists:flatten(io_lib:format("~s", [CurrentVertex])),
	Matches = ets:lookup(traffic_signals, CurrentVertexStr),

	case length(Matches) of
		0 -> move_to_next_vertex(State);
	 	_ -> 	
			case LastVertex of
				ok -> move_to_next_vertex(State);
				_ ->
					{_, TrafficSignalsPid} = lists:nth(1, Matches),
					class_Actor:send_actor_message(TrafficSignalsPid, {querySignalState, LastVertex}, State)
			end
	 end.



move_to_next_vertex( State ) ->
	[ CurrentVertex | [ NextVertex | Path ] ] = getAttribute( State , path ),
	Edge = list_to_atom(lists:concat([ CurrentVertex , NextVertex ])),

	DecrementVertex = getAttribute( State , last_vertex_pid ),

    LinkData = lists:nth(1, ets:lookup(list_streets , Edge)),
    {_, Id, Length, Capacity, _Freespeed, Occupation, _Lanes, _DR, IsCycleway, IsCyclelane, Inclination} = LinkData,

    Ocupation = if 
        % Cellsize do carro = 7,5 e cellsize da bike = 3
        % Cellsize é o comprimento do veículo mais a distância de segurança
        % Considera também que em uma faixa de carro passam duas bikes uma do lado da outra 
        % Na ciclovia ou ciclofaixa é uma bike atrás da outra
        (IsCycleway or IsCyclelane) -> 
            1/2.5;
        true ->
            1/5
    end,
		
	case DecrementVertex of
		ok -> ok;
		_ -> ets:update_counter( list_streets, DecrementVertex , { 6 , -Ocupation })
	end,	
	ets:update_counter( list_streets , Edge , { 6 , Ocupation }),
	
    PersonalSpeed = getAttribute( State , personal_speed ),
    NumberBikes = 1, % TODO obter NumberBikes
    Speed = traffic_models:get_speed_bike(PersonalSpeed, Length, Capacity, Occupation, NumberBikes, IsCycleway, IsCyclelane, Inclination), 
    Time = round((Length / Speed) + 1),
    Distance = round(Length),

	TotalLength = getAttribute( State , distance ) + Distance,
	StateAfterMovement = setAttributes( State , [
		{distance , TotalLength} , {bike_position , Id} , {last_vertex, CurrentVertex}, {last_vertex_pid , Edge} , {path , [NextVertex | Path]}] ), 

	% io:format("t=~p: ~p; ~p->~p ~n", [class_Actor:get_current_tick_offset(State), getAttribute(State, bike_name), CurrentVertex, NextVertex]),
	% io:format("~p Tick: ~p; ~p => ~p, Dist: ~p, Time: ~p, Avg. Speed: ~p, NextTick: ~p\n", 
	% 	[getAttribute( State , bike_name ), class_Actor:get_current_tick_offset( State ), CurrentVertex, NextVertex, Distance, Time, Distance / Time, class_Actor:get_current_tick_offset( StateAfterMovement ) + Time]),

	executeOneway( StateAfterMovement , addSpontaneousTick , class_Actor:get_current_tick_offset( StateAfterMovement ) + Time ).





-spec receive_signal_state(wooper:state(), tuple(), pid()) -> oneway_return().
receive_signal_state( State , {Color, TicksUntilNextColor}, _TrafficLightPid ) -> 
	case Color of
		red -> 
			% io:format("[~p] red (green in ~p)\n", [TrafficLightPid, TicksUntilNextColor]),
			% Act spontaneously when the traffic light is green again...
			executeOneway( State , addSpontaneousTick , class_Actor:get_current_tick_offset( State ) + TicksUntilNextColor );
		green -> 
			% io:format("Traffic signal is green, continuing movement...\n"),
			move_to_next_vertex(State)
	end.

 

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	% TODO: Why this is needed?
	StartTime = getAttribute( State , start_time ),
    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute( State , start_time , FirstActionTime ),
	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).

