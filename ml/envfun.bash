#!/bin/bash

if mosmlc -c -I /comp/105/lib envfun.sml ; then
  echo 'load "Unit"; load "envfun"; Unit.report(); quit();' |
        mosml -I /comp/105/lib -P full
  echo
fi
