FROM haskell:7.8

RUN apt-get install -y pkg-config \
    && echo "deb http://http.debian.net/debian jessie contrib main" >> /etc/apt/sources.list \
    && echo "deb-src http://http.debian.net/debian jessie contrib main" >> /etc/apt/sources.list \
    && apt-get update -o Dir::Etc::sourcelist="sources.list" \
        -o Dir::Etc::sourceparts="-" -o APT::Get::List-Cleanup="0" \
    && apt-get install -y --no-install-recommends libzmq3-dev

RUN cabal update

ADD ./monto-broker.cabal /root/monto-broker/monto-broker.cabal

RUN cd /root/monto-broker \
    && cabal sandbox init \
    && cabal install --dependencies-only -j4

ADD ./src /root/monto-broker/src
ADD ./broker /root/monto-broker/broker
ADD ./test /root/monto-broker/test

RUN cd /root/monto-broker \
    && cabal build -j4

ENV PATH /root/monto-broker/dist/build/broker/:$PATH
