-module(events_handler).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([listen_for_events/0, test/0]).

setup() ->
	{ _ , Pwd } = file:get_cwd(),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

	Paths = [ AmqpClientPath,
			string:concat( AmqpClientPath, "/ebin" ),
			string:concat( AmqpClientPath, "/include/rabbit_common/ebin" )
		],

    code:add_pathsa( Paths ).

test() ->
    setup(),

    %% Start a network connection
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),

    X = <<"traffic_sign">>,
    BindKey = <<"#">>,

    %% Declare a queue
    #'queue.declare_ok'{queue = Q}
        = amqp_channel:call(Channel, #'queue.declare'{queue = <<"my_queue">>}),

    ExchangeDeclare = #'exchange.declare'{exchange = X, type = <<"topic">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = BindKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% Publish a message
    Payload = <<"foobar bla bla bla">>,
    RoutingKey = <<"test.traffic.event">>,
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
    io:format("ENVIADO A PRIMEIRA MSG\n"),

    Payload1 = <<"vamos ver">>,
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload1}),
    io:format("ENVIADO A SEGUNDA MSG\n"),
    
    % Get the message back from the queue
    %Get = #'basic.get'{queue = Q, no_ack = true},
    %{#'basic.get_ok'{}, Content}
    %     = amqp_channel:call(Channel, Get),

    %%% Do something with the message payload
    %io:format("CONTENT 1: ~w\n", [element(3,Content)]),
    %%% (some work here)

    %%% Get the message back from the queue
    %{#'basic.get_ok'{}, Content1}
    %     = amqp_channel:call(Channel, Get),

    %%% Do something with the message payload
    %io:format("CONTENT 2: ~w\n", [element(3,Content1)]),
    % (some work here)

    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),

    ok.

listen_for_events() ->
	setup(),
	io:format("LISTENING FOR EVENTS\n"),

    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    X = <<"traffic_sign">>,
    BindKey = <<"#">>,

    #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, #'queue.declare'{}),

    ExchangeDeclare = #'exchange.declare'{exchange = X, type = <<"topic">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = BindKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    Sub = #'basic.consume'{queue = Q, no_ack = true},
    #'basic.consume_ok'{} = amqp_channel:call(Channel, Sub),

    loop(Channel).


loop(Channel) ->
    receive
        {#'basic.deliver'{ exchange=_Exchange, routing_key=RoutingKey}, Content} ->
            io:format("RoutingKey received: ~p~n", [RoutingKey]),
            #amqp_msg{payload = Payload} = Content,
            io:format("Payload received: ~p~n", [Payload]),
			ets:insert(traffic_events, {nodeID, Payload}),
            loop(Channel)
    end.
