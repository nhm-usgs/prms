#!/bin/bash 

BASELINE="output_baseline"

CHECKDIR="output"

for ff in ${BASELINE}/*; do
    filename=$(basename ${ff})
    # echo "---------- ${filename} ----------"
    numdiff -U --separators=', \n' -a 0.001 -r 0.001 ${ff} ${CHECKDIR}/${filename}
    #diff -q --ignore-all-space -s ${ff} ${CHECKDIR}/${filename}
done
