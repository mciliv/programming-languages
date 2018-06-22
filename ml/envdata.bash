#!/bin/bash

if mosmlc -c -I /comp/105/lib envdata.sml ; then
  echo 'load "Unit"; load "envdata"; Unit.report(); quit();' |
        mosml -I /comp/105/lib -P full
  echo
fi
