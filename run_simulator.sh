rm -fr src/Sim-Diasca*
rm -f src/*.traces

cd src

make all

make smart_city_run CMD_LINE_OPT="--batch"
