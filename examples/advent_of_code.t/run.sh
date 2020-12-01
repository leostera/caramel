#!/bin/bash -e

echo "== Caramel =="
caramelc compile *.ml
erlc *.erl
echo "OK"
echo ""

escript runner.erl
