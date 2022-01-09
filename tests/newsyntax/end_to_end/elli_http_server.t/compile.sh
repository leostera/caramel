#!/bin/bash -e

mkdir -p ebin
pushd src;
caramel compile --new-syntax erlang.caramel
caramel compile --new-syntax http_method.caramel
caramel compile --new-syntax elli.caramel
caramel compile --new-syntax main_sup.caramel main_app.caramel main.caramel
erlc *.core
mv *.beam ../ebin/
popd;
