#!/bin/bash

if mosmlc -c -I /comp/105/lib envboth.sml ; then
  echo 'load "Unit"; load "envboth"; Unit.report(); quit();' |
        mosml -I /comp/105/lib -P full
  echo
fi
