#
# Dockerfile for creating an ubuntu machine with MLKit and SMLtoJs
# installed.
#
# Build it with:
#
#  docker build -t mlkit .
#
# Run the image with:
#
#  docker run -it mlkit
#

FROM ubuntu:21.10

RUN apt-get update && apt-get install -y \
    gcc gcc-multilib autoconf make

ADD https://github.com/melsman/mlkit/releases/latest/download/mlkit-bin-dist-linux.tgz .

RUN cd / && tar xzf mlkit-bin-dist-linux.tgz

RUN cd /mlkit-bin-dist-linux && make install

RUN mkdir /usr/local/etc/mlkit && \
    mkdir /usr/local/etc/smltojs && \
    echo "SML_LIB /usr/local/lib/mlkit" > /usr/local/etc/mlkit/mlb-path-map && \
    echo "SML_LIB /usr/local/lib/smltojs" > /usr/local/etc/smltojs/mlb-path-map

RUN rm -rf /mlkit-bin-dist-linux.tgz /mlkit-bin-dist-linux
