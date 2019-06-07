FROM erlang:20

RUN apt update && apt install -y \
  make \
  uuid-runtime 

ADD . /src
RUN cd /src && make all

RUN cd /src/mock-simulators/smart_city_model/src && make all

CMD [ "make", "smart_city_run", "CMD_LINE_OPT='--batch'" ]

#docker run -t -w /src/mock-simulators/smart_city_model/src -h teste.com -v /Users/eduardosantana/volume:/src/mock-simulators/smart_city_model/output -e USER=root interscsimulator