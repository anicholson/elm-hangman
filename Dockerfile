FROM agrafix/docker-haskell-elm:latest

MAINTAINER Andy Nicholson <andrew@anicholson.net>

RUN apt-get install -yy lighttpd
RUN useradd www

RUN mkdir -p /opt/app/src
RUN mkdir -p /opt/app/build

ADD . /opt/app/src

WORKDIR /opt/app/src

RUN elm package install -y

RUN elm make src/App.elm --output ./elm.js

RUN cp ./index.html ../build/
RUN cp ./style.css ../build/
RUN cp ./elm.js ../build/

WORKDIR /opt/app

RUN chown -R www build

RUN chmod -R 0400 build

RUN lighttpd -t -f src/lighttpd.conf

EXPOSE 80

CMD ["lighttpd", "-D", "-f", "src/lighttpd.conf"]

