FROM codesimple/elm:0.17

MAINTAINER Andy Nicholson <andrew@anicholson.net>

RUN apt-get update
RUN apt-get install -yy lighttpd
RUN useradd www

RUN mkdir -p /opt/app

ADD . /opt/app

WORKDIR /opt/app

RUN elm package install -y

RUN elm make src/App.elm --output ./dist/elm.js

WORKDIR /opt/app/dist

RUN chown -R www . 

RUN lighttpd -t -f ../lighttpd.conf

EXPOSE 80

ENTRYPOINT ["lighttpd", "-D", "-f", "/opt/app/lighttpd.conf"]
