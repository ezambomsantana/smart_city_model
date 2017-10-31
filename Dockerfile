#InterSCSimulator Dockerfile
FROM debian:unstable

RUN apt update && apt install make erlang -qy

RUN apt install uuid-runtime

ADD . /src
RUN cd /src && make all


CMD [ "make", "smart_city_run", "CMD_LINE_OPT='--batch'" ]

#building: sudo docker build -t simulador/latest .
#running: sudo docker run -t -w /src/mock-simulators/smart_city_model/src --net="host" -v /home/eduardo/volume:/src/mock-simulators/smart_city_model/output -e USER=root ezambomsantana/interscsimulator
