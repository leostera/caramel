#!/bin/bash -e

CARAMELC="../../_build/default/bin/caramelc.exe"

for d in expressions ffi functions modules types otp processes; do
  pushd $d > /dev/null;
    echo -e "================================================"
    echo -e "= Building: $d"
    echo -e "================================================"
    echo -e ""
    rm -f ./*.cm* ./*.erl ./*.beam
    ${CARAMELC} compile *.mli *.ml

    erlc *.erl || true
    erlc +to_core *.erl || true

    echo -e "\n\n"

  popd > /dev/null;
done
