# InterSCSimulator

It is the official repository of the InterSCSimulator, a large-scale, smart city simulator. InterSCSimulator is based on Sim-Diasca, an general purpose simulator implemented in Erlang.

# Configuration

To run the simulator you need to create a configuration file in the root
directory called `interscsimulator.conf`. This file should contain the path to
the config.xml. By default you can point to example/config.xml in the root of
the source tree.

`$(pwd)/example/config.xml`

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
