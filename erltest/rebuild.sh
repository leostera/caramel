#!/bin/bash -e

CARAMELC="../../caramelc"

for d in expressions ffi functions modules types otp processes; do
  pushd $d;
    rm -f ./*.cm* ./*.erl ./*.beam
    ${CARAMELC} -c $(${CARAMELC} -depend -sort *.mli *.ml)
    erlc *.erl || true
  popd;
done
