#!/bin/bash -e

CARAMELC="../../caramelc"

for d in expressions ffi functions modules types otp processes; do
  pushd $d > /dev/null;
    echo -e "================================================"
    echo -e "= Building: $d"
    echo -e "================================================"
    echo -e ""
    rm -f ./*.cm* ./*.erl ./*.beam
    ${CARAMELC} -c $(${CARAMELC} -depend -sort *.mli *.ml)

    erlc *.erl || true

    echo -e "\n\n"

  popd > /dev/null;
done
