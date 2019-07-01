-module(traffic_models).

-export([get_speed_car/2, get_speed_walk/2]).

% There is DR in link and car can use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}}, true) ->
	link_density_speed(Id, Length, RawCapacity, NumberCars, Freespeed, Lanes);

% There is DR but not effective:
get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}}, noeffect) ->
	get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {}}, noeffect);

% There is DR but car cannot use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}}, false) ->
	link_density_speed(Id, Length, RawCapacity, NumberCars, Freespeed, Lanes);

% There is no DR:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {}}, _) ->
	link_density_speed(Id, Length, RawCapacity, NumberCars, Freespeed, Lanes).

link_density_speed(Id, Length, Capacity, NumberCars, Freespeed, _Lanes) ->

	Alpha = 1,
	Beta = 1,
	Speed = case NumberCars >= Capacity of
		true -> 1.0;
		false -> Freespeed * math:pow(1 - math:pow((NumberCars / Capacity), Beta), Alpha)
	end,

	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)}.

get_speed_walk(LinkData, _) ->
	{_, Id, Length, _, _, _} = LinkData,	
	Time = ( Length / 2 ),

	{Id, ceil(Time), round(Length)}.