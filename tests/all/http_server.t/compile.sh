#!/bin/bash -e

mkdir ebin
pushd src;
caramel compile erlang.ml
caramel compile elli.ml
caramel compile http_method.ml http_status.ml
caramel compile main_sup.ml main_app.ml main.ml
erlc *.core
mv *.beam ../ebin/
popd;
