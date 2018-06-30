FROM erlang:20

RUN apt update && apt install -y \
  make \
  uuid-runtime 

RUN mkdir /interscsimulator
ADD . /interscsimulator
WORKDIR /interscsimulator
RUN make all

WORKDIR mock-simulators/smart_city_model/src
RUN make all

ENV USER root

VOLUME /interscsimulator/mock-simulators/smart_city_model/input
VOLUME /interscsimulator/mock-simulators/smart_city_model/output

CMD ["make", "smart_city_run", "CMD_LINE_OPT='--batch'"]
