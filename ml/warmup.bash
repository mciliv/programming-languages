#!/bin/bash

if mosmlc -c -I /comp/105/lib warmup.sml ; then
  echo 'load "Unit"; load "warmup"; Unit.report(); quit();' |
        mosml -I /comp/105/lib -P full
  echo
fi
