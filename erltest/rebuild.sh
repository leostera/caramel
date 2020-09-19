#!/bin/bash -e

CARAMELC="../../caramelc -c"

rm -f ./**/*.cm* ./**/*.erl ./**/*.beam

pushd ./expressions;
  ${CARAMELC} *.ml
popd;

pushd ./ffi;
  ${CARAMELC} *.ml
popd;

pushd ./functions;
  ${CARAMELC} io.ml *.mli *.ml
popd

pushd ./modules;
  ${CARAMELC} *.mli *.ml
popd

pushd ./types;
  ${CARAMELC} *.mli record.ml type_args.ml *.ml
popd

pushd ./otp;
  ${CARAMELC} timer.ml erlang.ml gen_server.ml *.ml
popd

pushd ./processes;
  ${CARAMELC} io.ml erlang.ml process.ml *.ml
popd
