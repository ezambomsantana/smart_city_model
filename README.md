# InterSCSimulator #
This is the official repository of InterSCSimulator, a large-scale smart city simulator. InterSCSimulator is based on Sim-Diasca, a general purpose simulator implemented in Erlang.

## Running InterSCSimulator with Docker ##
Download and extract Sim-Diasca. Place this repository under Sim-Diasca's `mock-simulators` directory:
```
cd sim-diasca/mock-simulators
git clone https://github.com/ezambomsantana/smart_city_model.git
```

Under InterSCSimulator's directory, create a configuration file called `interscsimulator.conf` with the path to `config.xml` for the desired simulation scenario:
```
cd smart_city_model
echo "../simple_scenario/config.xml" > interscsimulator.conf
```

Go back to Sim-Diasca root directory and build the image:
```
cd ../..
docker build -f mock-simulators/smart_city_model/Dockerfile -t interscitysimulator .  
```

Create a Docker network:
```
docker network create interscity
```

Run the simulator container mouting a volume from the desired scenario directory:
```
docker run -it --network interscity --hostname interscity.local -v $(pwd)/mock-simulators/smart_city_model/simple_scenario/:/interscsimulator/mock-simulators/smart_city_model/simple_scenario interscitysimulator
```

## Running InterSCSimulator on Linux ##
### Prerequisites ### 
#### Sim-Diasca and Erlang ####
Download and extract Sim-Diasca. If you don't have Erlang installed, you can use the script at `sim-diasca/common/conf/install-erlang.sh`.
After Erlang is installed, compile Sim-Diasca:
```
cd sim-diasca
make all
```
3. Make sure that the hostname is a FQDN, e.g. `localhost.local`

#### RabbitMQ ####
Run the `install-deps.sh` script. Dependencies will be placed in the `lib/` directory.

### Installation ###
Clone this respository under Sim-Diasca's `mock-simulators` directory:
```
cd mock-simulators
git clone https://github.com/ezambomsantana/smart_city_model.git
``` 

Compile InterSCSimulator:
```
cd smart_city_model/src
make all
```

---

### Run InterSCSimulator ###
Under InterSCSimulator's directory, create a configuration file called `interscsimulator.conf` with the path to `config.xml` for the desired simulation scenario:
```
cd sim-diasca/mock-simulators/smart_city_model
echo "../simple_scenario/config.xml" > interscsimulator.conf
```

Run the simulator
```
cd src
make smart_city_run CMD_LINE_OPT="--batch"
```


#### Visualizing the simulation with OTFVis ####

Visualizing simulations
===

InterSCity input and output files are compatible with [MATSim](https://www.matsim.org/) formats, so users can take advantage of the extensive tooling already developed by MATSim contributors. The following tutorial shows how to use [OTFVis](https://www.matsim.org/extension/otfvis) for visualization.

### Get MATsim and OTFvis ###
1. Download the standalone zip for the latest stable release from https://www.matsim.org/downloads/. Download the zip for OTFVis from https://github.com/matsim-org/matsim/releases/tag/matsim-0.9.0.

2. Extract:
```
unzip matsim-0.9.0.zip 
unzip otfvis-0.9.0.zip 
```

### Create .mvi file for your scenario ###
1. After running the simulation, create a copy of `events.xml` from your scenario and compress it:
```
cd mock-simulators/smart_city_model/scenario
cp events.xml scenario.events.xml
gzip scenario.events.xml
``` 

2. Use OTFvis to generate a .mvi file from your events and network file:
```
java -cp matsim-0.9.0/matsim-0.9.0.jar:otfvis-0.9.0/otfvis-0.9.0.jar org.matsim.contrib.otfvis.RunOTFVis --convert <path_to_scenario.events.xml.gz> <path_to_network.xml> scenario.mvi <time_resolution_in_seconds>
```

### Visualize the simulation ###
Finally, open the .mvi file with OTFvis:
```
java -cp matsim-0.9.0/matsim-0.9.0.jar:otfvis-0.9.0/otfvis-0.9.0.jar org.matsim.contrib.otfvis.RunOTFVis scenario.mvi
```