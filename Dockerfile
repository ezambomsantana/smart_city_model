#InterSCSimulator Dockerfile
FROM erlang:20-alpine

RUN apk add --update make

RUN apk add util-linux-dev

ADD . /src
RUN cd /src && make all


CMD [ "make", "smart_city_run", "CMD_LINE_OPT='--batch'" ]

#RUN sudo docker run -t -w /src/mock-simulators/smart_city_model/src --net="host" -v /home/eduardo/volume2:/src/mock-simulators/smart_city_model/output -e USER=root image