# InterSCSimulator #
This is the official repository of InterSCSimulator, a large-scale smart city simulator. InterSCSimulator is based on Sim-Diasca, a general purpose simulator implemented in Erlang.

**Note:** This version of InterSCSimulator is compatible with Sim-Diasca version 2.2.11-rc4.

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

Run with Docker Compose:
```
docker compose up --build
```

This will build the InterSCSimulator image and start both the simulator and RabbitMQ services automatically.

To stop the services and clean up:
```
docker compose down
```

This will stop and remove the containers, networks, and volumes created by the compose file.

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
