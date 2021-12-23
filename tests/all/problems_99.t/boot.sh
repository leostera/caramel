#!/bin/zsh

for ml in *.ml; do
  dir="$(basename $ml .ml).t"
  mkdir -p $dir;
  cp $ml $dir;
  cp run.t $dir/run.t;
done
