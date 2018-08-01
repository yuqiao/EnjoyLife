FROM ubuntu:14.04
MAINTAINER yuqiao "yunqiao2014@163.com"

ENV REFRESHED_AT 2018-08-01
RUN apt-get update
RUN apt-get -y -q install nginx
RUN mkdir -p /var/www/html
RUN mv /etc/apt/sources.list /etc/apt/sources.list.bak
ADD sources.list /etc/apt/sources.list
ADD nginx/global.conf /etc/nginx/conf.d/
ADD nginx/nginx.conf  /etc/nginx/nginx.conf
EXPOSE 80
