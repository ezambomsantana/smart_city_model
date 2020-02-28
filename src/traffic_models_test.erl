-module(traffic_models_test).

-export([get_speed_bike_tests/0]).


%%%%%%%%%%%%%%%%%
% TESTES %%%%%%%%
%%%%%%%%%%%%%%%%%
% Para executar o teste:
% erlc traffic_models.erl
% erl -noshell -s traffic_models get_speed_bike_test -s init stop

get_speed_bike_tests() ->
    io:format("~n~nExecutando testes~n~n"),
    io:format("----------------------~n"),
    test_cicleway_speed_must_be_greater_than_ciclelane_speed(),
    test_mixed_traffic_speed_must_be_greater_than_cycleway_speed(),
    test_descent_speed_must_be_greater_than_plane_speed(),
    test_plane_speed_must_be_greater_than_climb_speed(),
    test_speed_is_greater_for_less_occupation_when_cicleway(),
    test_speed_is_greater_for_less_occupation_when_mixed_traffic(),
    test_speed_is_1ms_for_saturated_link_when_cicleway(),
    test_speed_is_1ms_for_saturated_link_when_mixed_traffic(), 
    test_personal_speed_should_be_probabilistic(),
    io:format("~nTestes executados =)~n").



test_cicleway_speed_must_be_greater_than_ciclelane_speed() ->

    CaseTest = "Velocidade em ciclovia tem que ser maior que velocidade em ciclofaixa",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 0,
    NumberBikes = 15,
    IsCycleway = true,
    IsCyclelane = false,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,

    SpeedInCicleway = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedInCicleway),

    IsCycleway2 = false,
    IsCyclelane2 = true,
    Occupation2 = occupation_for(NumberCars, NumberBikes, IsCycleway2, IsCyclelane2),

    SpeedInCiclelane = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation2, IsCycleway2, IsCyclelane2, Inclination),
    assertReasonableSpeed(SpeedInCiclelane),

    assertXGreaterThanY(SpeedInCicleway, SpeedInCiclelane),
    
    fimDoTestCase().


occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane) ->
    if
        IsCycleway or IsCyclelane ->
            NumberBikes/2.5;
        true ->
            NumberCars + NumberBikes/5
    end.


test_mixed_traffic_speed_must_be_greater_than_cycleway_speed() ->

    CaseTest = "Velocidade em tráfego misto tem que ser maior que velocidade em ciclovia (!!!)",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 10,
    NumberBikes = 5,
    IsCycleway = false,
    IsCyclelane = false,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,    

    SpeedInMixedTraffic = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedInMixedTraffic),

    IsCycleway2 = true,
    IsCyclelane2 = false,

    SpeedInCycleway = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway2, IsCyclelane2, Inclination),
    assertReasonableSpeed(SpeedInMixedTraffic),

    assertXGreaterThanY(SpeedInMixedTraffic, SpeedInCycleway),
    
    fimDoTestCase().





test_descent_speed_must_be_greater_than_plane_speed() ->

    CaseTest = "Velocidade na descida tem que ser maior que velocidade no plano",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 0,
    NumberBikes = 15,
    IsCycleway = false,
    IsCyclelane = true,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = (700 - 760)/10,

    SpeedInDescent = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedInDescent),

    Inclination2 = 0,

    SpeedInPlane = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination2),
    assertReasonableSpeed(SpeedInPlane),

    assertXGreaterThanY(SpeedInDescent, SpeedInPlane),
    
    fimDoTestCase().




test_plane_speed_must_be_greater_than_climb_speed() ->

    CaseTest = "Velocidade no plano tem que ser maior que velocidade na subida",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 0,
    NumberBikes = 15,
    IsCycleway = false,
    IsCyclelane = true,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,

    SpeedInPlane = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedInPlane),

    Inclination2 = (780 - 760)/10,

    SpeedInClimb = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination2),
    assertReasonableSpeed(SpeedInClimb),

    assertXGreaterThanY(SpeedInPlane, SpeedInClimb),
    
    fimDoTestCase().










test_speed_is_greater_for_less_occupation_when_cicleway() ->

    CaseTest = "Velocidade tem que ser menor para vias mais ocupadas, quando via é ciclovia",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 2,
    NumberBikes = 5,
    IsCycleway = true,
    IsCyclelane = false,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,

    SpeedWithLessOccupation = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedWithLessOccupation),

    NumberCars2 = 30,
    NumberBikes2 = 40,
    Occupation2 = occupation_for(NumberCars2, NumberBikes2, IsCycleway, IsCyclelane),

    SpeedWithMoreOccupation = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation2, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedWithMoreOccupation),

    assertXGreaterThanY(SpeedWithLessOccupation, SpeedWithMoreOccupation),
    
    fimDoTestCase().


test_speed_is_greater_for_less_occupation_when_mixed_traffic() ->

    CaseTest = "Velocidade tem que ser menor para vias mais ocupadas, quando via é com tráfego misto",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 2,
    NumberBikes = 5,
    IsCycleway = false,
    IsCyclelane = false,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,

    SpeedWithLessOccupation = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedWithLessOccupation),

    NumberCars2 = 30,
    NumberBikes2 = 40,
    Occupation2 = occupation_for(NumberCars2, NumberBikes2, IsCycleway, IsCyclelane),

    SpeedWithMoreOccupation = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation2, IsCycleway, IsCyclelane, Inclination),
    assertReasonableSpeed(SpeedWithMoreOccupation),

    assertXGreaterThanY(SpeedWithLessOccupation, SpeedWithMoreOccupation),
    
    fimDoTestCase().




test_speed_is_1ms_for_saturated_link_when_cicleway() ->

    CaseTest = "Velocidade tem que ser 1m/s em via saturada, quando via é ciclovia",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 0,
    NumberBikes = 501,
    IsCycleway = true,
    IsCyclelane = false,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,

    SpeedInSaturatedLink = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertEquals(SpeedInSaturatedLink, 1),
    
    fimDoTestCase().








test_speed_is_1ms_for_saturated_link_when_mixed_traffic() ->

    CaseTest = "Velocidade tem que ser 1m/s em via saturada, quando via é em tráfego misto",
    io:format("Executando teste ~p~n", [CaseTest]),

    PersonalSpeed = 12/3.6,
    Capacity = 100,
    NumberCars = 90,
    NumberBikes = 51,
    IsCycleway = false,
    IsCyclelane = false,
    Occupation = occupation_for(NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Inclination = 0,

    SpeedInSaturatedLink = traffic_models:get_speed_bike(PersonalSpeed, Capacity, Occupation, IsCycleway, IsCyclelane, Inclination),
    assertEquals(SpeedInSaturatedLink, 1),
    
    fimDoTestCase().











test_personal_speed_should_be_probabilistic() ->

    CaseTest = "Personal speed should be probabilistic",
    io:format("Executando teste ~p~n", [CaseTest]),

    Speed1 = traffic_models:get_personal_bike_speed(),
    assertReasonableSpeed(Speed1),
    Speed2 = traffic_models:get_personal_bike_speed(),
    assertReasonableSpeed(Speed2),
    Speed3 = traffic_models:get_personal_bike_speed(),
    assertReasonableSpeed(Speed3),

    assertNotEquals(Speed1, Speed2),
    assertNotEquals(Speed1, Speed3),
    assertNotEquals(Speed2, Speed3),
    
    fimDoTestCase().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Daqui pra frente, arcabouço de testes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fimDoTestCase() ->
    io:format("--------------~n").

assertReasonableSpeed(VelocidadeBike) ->
    if
        (VelocidadeBike < 1/3.6) or (VelocidadeBike > 30/3.6) ->
            io:format("TESTE FALHOU!!! =( ~nVelocidade devia ser entre 1km/h e 30km/h, mas foi ~wkm/h~n", [VelocidadeBike*3.6]);
        true ->
            naoImporta
    end.

assertEquals(Value, Expected) ->
    if
        Value /= Expected ->
            io:format("TESTE FALHOU!!! =( ~nValor deveria ser ~w, mas foi ~w~n", [Expected, Value]);
        true ->
            io:format("") % não importa
    end.

assertXGreaterThanY(X, Y) ->
    if
        X =< Y ->
            io:format("TESTE FALHOU!!! =( ~nValor deveria ser maior que ~w, mas foi ~w~n", [Y, X]);
        true ->
            io:format("") % não importa
    end.

assertNotEquals(Value1, Value2) ->
    if
        Value1 == Value2 ->
            io:format("TESTE FALHOU!!! =( ~nValores deveriam ser diferentes, mas foral iguais: ~w~n", [Value1]);
        true ->
            io:format("") % não importa
    end.

