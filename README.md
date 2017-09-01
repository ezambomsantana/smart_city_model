# smart_city_model
InterSCSimulator code repository

# Run InterSCSimulator

In sim-diasca's root directory run:

`$ make all`

Enter in mock-simulator/smart_city_model/src and run:

`$ make smart_city_run CMD_LINE_OPT="--batch"`

## Tips

* Install erlang via this script: sim-diasca/common/conf/install-erlang.sh
* Configure hostname FQDN to something like this: localhost.org

## Open Issues

* Wrong link after a bus trip
* Create the entire metro network of Sao Paulo
* Include the instantaneous speed of the cars in each link
