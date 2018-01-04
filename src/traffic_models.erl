-module(traffic_models).

-export([ 
   get_speed_car/1,
   get_speed_walk/1
  ]).

get_speed_car( Data ) ->

	Id = element( 2 , Data ), % Link Id
	Length = element( 3 , Data ), % Link Length	
	Capacity = element( 4 , Data )  / 2,
	Freespeed = element( 5 , Data ), 	
	NumberCars = element( 6 , Data ), 

	% Calculate car speed
	Density = ( NumberCars ),

	MaximumDensity = Capacity,

	MinimumDensity = (Capacity * 0.3) 	 ,

	Speed = case Density > MinimumDensity of

		true ->

			case Density >= MaximumDensity of

				true ->
					0.8;
				false ->
			
					Number = math:pow ( 1 - ( Density / MaximumDensity ) , 0.6),
					Freespeed * (Number)
	
			end;
			
		false ->
		
			Freespeed 

	end,

	Time = ( Length / Speed ) + 1,

	{ Id , round( Time ) , round ( Length ) }.


get_speed_walk( Data ) ->

	Id = element( 2 , Data ), % Link Id
	Length = element( 3 , Data ), % Link Length	

	Time = ( Length / 2 ) + 1,

	{ Id , round( Time ) , round ( Length ) }.
