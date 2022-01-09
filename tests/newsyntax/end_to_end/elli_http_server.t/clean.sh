#!/bin/bash -e

rm -rf _build ebin/*.beam
pushd src;
rm -f *.beam *.core *.meta *.cmi
popd;
