#!/bin/bash

##SBATCH --dependency=afterany:

#SBATCH --time=12:00:00

#SBATCH --ntasks-per-node=1

## RUN THIS IN THE model_out DIR ##

runs=('TopoEns_0x0_LPaERSM40_a')
##runs+=('TopoEns_0x10_P2SNoM40_a')
runs+=('TopoEns_0x20_P2SNoM40_a')
runs+=('TopoEns_0x30_P2SNoM40_a')
runs+=('TopoEns_0x40_P2SNoM40_a')
runs+=('TopoEns_0x50_P2SNoM40_a')
runs+=('TopoEns_22x0_LPaERSM40_a')
runs+=('TopoEns_22x10_P2SNoM40_a')
runs+=('TopoEns_22x20_P2SNoM40_a')
runs+=('TopoEns_22x30_P2SNoM40_a')
runs+=('TopoEns_22x40_P2SNoM40_a')
runs+=('TopoEns_22x50_P2SNoM40_a')
runs+=('TopoEns_45x0_LPaERSM40_a')
runs+=('TopoEns_45x10_P2SNoM40_a')
runs+=('TopoEns_45x20_P2SNoM40_a')
runs+=('TopoEns_45x30_P2SNoM40_a')
runs+=('TopoEns_45x40_P2SNoM40_a')
runs+=('TopoEns_45x50_P2SNoM40_a')
runs+=('TopoEns_60x0_LPaERSM40_a')
runs+=('TopoEns_60x10_P2SNoM40_a')
runs+=('TopoEns_60x20_P2SNoM40_a')
##runs+=('TopoEns_60x30_P2SNoM40_a')
runs+=('TopoEns_60x40_P2SNoM40_a')
runs+=('TopoEns_60x50_P2SNoM40_a')
runs+=('TopoEns_90x0_LPaERSM40_a')
runs+=('TopoEns_90x10_P2SNoM40_a')
runs+=('TopoEns_90x20_P2SNoM40_a')
runs+=('TopoEns_90x30_P2SNoM40_a')
runs+=('TopoEns_90x40_P2SNoM40_a')
runs+=('TopoEns_90x50_P2SNoM40_a')
#runs+=('TopoEns_aqua_P2SNoM40_a')

mkdir topoSumFiles

for irun in ${runs[*]}
do
    cd $irun
    sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}{0096,0097,0098,0099,0100}.acc${irun}.nc
    sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}{0091,0092,0093,0094,0095,0096,0097,0098,0099,0100}.acc${irun}.nc
    scaleacc ANN0096* aij
    scaleacc ANN0091* aij
    mv ANN* /discover/nobackup/projects/giss_ana/users/dmglaser/model_out/topoSumFiles/
    cd /discover/nobackup/projects/giss_ana/users/dmglaser/model_out
done