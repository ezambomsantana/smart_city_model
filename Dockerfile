FROM debian:unstable

RUN apt update && apt install -y \
  make \
  uuid-runtime \
  erlang

WORKDIR /interscsimulator
COPY . .
RUN rm -r mock-simulators/smart_city_model
RUN make all

COPY mock-simulators/smart_city_model mock-simulators/smart_city_model
WORKDIR mock-simulators/smart_city_model/src
RUN make all

ENV USER root
CMD [ "make", "smart_city_run", "CMD_LINE_OPT='--batch'" ]
