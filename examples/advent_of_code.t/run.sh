#!/bin/bash -e

caramelc compile *.ml
erlc *.erl
escript runner.erl
