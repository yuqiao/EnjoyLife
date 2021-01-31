FROM ubuntu:14.04
MAINTAINER juedaiyuer "juedaiyuer@gmail.com"
ENV REFRESHED_AT 2016-07-10

RUN apt-get update
RUN apt-get install -y ruby ruby-dev build-essential redis-tools
RUN gem install --no-rdoc --no-ri sinatra json redis

RUN mkdir -p /opt/webapp

EXPOSE 4567

CMD [ "/opt/webapp/bin/webapp" ]
