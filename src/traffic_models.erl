-module(traffic_models).

-export([ 
   get_speed_car/1,
   get_speed_walk/1
  ]).

get_speed_car( LinkData ) ->

	{ _ , Id , Length , Capacity , Freespeed , NumberCars } = LinkData,

	MinimumDensity = (Capacity * 0.3) 	 ,
	Speed = case NumberCars > MinimumDensity of

		true ->
			case NumberCars >= Capacity of
				true ->
					0.8;
				false ->
					Freespeed * math:pow ( 1 - ( NumberCars / Capacity ) , 0.6)
			end;
		false ->
			Freespeed
	end,

	Time = ( Length / Speed ) + 1,
	{ Id , round( Time ) , round ( Length ) }.


get_speed_walk( LinkData ) ->

	{ _ , Id , Length , _ , _ , _ } = LinkData,	

	Time = ( Length / 2 ) + 1,

	{ Id , round( Time ) , round ( Length ) }.
