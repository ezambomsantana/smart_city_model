# InterSCSimulator #
This is the official repository of InterSCSimulator, a large-scale smart city simulator. InterSCSimulator is based on Sim-Diasca, a general purpose simulator implemented in Erlang.

## Getting started ##
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
echo "$(pwd)/sp_completo/config.xml" > interscsimulator.conf
```

Run the simulator
```
cd src
make smart_city_run CMD_LINE_OPT="--batch"
```
