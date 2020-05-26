#!/bin/bash

PYSIM="/home/wierton/Documents/PhD-TA/CompilerLab-2020/projects/Project 3/irsim-py/irsim-py/cli/irsim_py35.py"
for i in workspace/*; do
  a=$(python3 "$PYSIM" $i)
  b=$(build/irsim $i)
  if [ s"$a" != s"$b" ]; then
    echo FAIL at $i
    exit 0
  fi
done
