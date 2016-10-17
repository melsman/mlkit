# This simple Dockerfile demonstrates a minimal configuration of a
# 64-bit Ubuntu machine necessary to build MLKit.

FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
    gcc gcc-multilib mlton autoconf make

ADD . /mlkit

RUN cd /mlkit && ./autobuild && ./configure 
RUN cd /mlkit && make mlkit
# RUN cd /mlkit && make bootstrap
RUN cd /mlkit && make mlkit_libs
RUN cd /mlkit && make install
