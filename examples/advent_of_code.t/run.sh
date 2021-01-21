#!/bin/bash -e

echo "== Caramel =="
caramel compile *.ml
erlc *.erl
echo "OK"
echo ""

escript runner.erl
