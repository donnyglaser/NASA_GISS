#!/bin/bash

# run in nobackup model_out

list=`ls -d TopoEns_*_a`

for ilist in $list
do
        cd $ilist
        cp /discover/home/dmglaser/modelE/decks/scripts/eqAnalysis.* .
        sbatch eqAnalysis.sh
        cd ..
done