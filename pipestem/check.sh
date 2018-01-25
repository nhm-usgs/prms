#!/bin/bash 

BASELINE="output_baseline"

CHECKDIR="output"

for ff in ${BASELINE}/*; do
    filename=$(basename ${ff})
    echo "---------- ${filename} ----------"
    diff -s ${ff} ${CHECKDIR}/${filename}
done
