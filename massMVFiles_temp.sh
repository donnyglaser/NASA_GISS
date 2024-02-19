#!/bin/bash

## execute this in the 

runs=('TopoEns_0x0_LPaERSM40_a')
#runs+=('TopoEns_0x10_P2SNoM40_a')
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
#runs+=('TopoEns_45x10_P2SNoM40_a')
runs+=('TopoEns_45x20_P2SNoM40_a')
#runs+=('TopoEns_45x30_P2SNoM40_a')
runs+=('TopoEns_45x40_P2SNoM40_a')
runs+=('TopoEns_45x50_P2SNoM40_a')
runs+=('TopoEns_60x0_LPaERSM40_a')
runs+=('TopoEns_60x10_P2SNoM40_a')
runs+=('TopoEns_60x20_P2SNoM40_a')
#runs+=('TopoEns_60x30_P2SNoM40_a')
runs+=('TopoEns_60x40_P2SNoM40_a')
runs+=('TopoEns_60x50_P2SNoM40_a')
runs+=('TopoEns_90x0_LPaERSM40_a')
runs+=('TopoEns_90x10_P2SNoM40_a')
runs+=('TopoEns_90x20_P2SNoM40_a')
runs+=('TopoEns_90x30_P2SNoM40_a')
runs+=('TopoEns_90x40_P2SNoM40_a')
runs+=('TopoEns_90x50_P2SNoM40_a')
runs+=('TopoEns_aqua_P2SNoM40_a')



for irun in "${runs[@]}"
do
    cd /discover/home/dmglaser/modelE/decks/
    cd $irun
    sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}{0100,0099,0098,0097,0096}.acc${irun}.nc
    scaleacc ANN0096-0100.acc${irun}.nc aij
    scaleacc ANN0096-0100.acc${irun}.nc oijl
    cd /discover/home/dmglaser/modelE/decks/tempTOPO/
    temp="/discover/home/dmglaser/modelE/decks/${irun}/ANN0096-0100.aij${irun}.nc"
    cp $temp .
    temp="/discover/home/dmglaser/modelE/decks/${irun}/ANN0096-0100.oijl${irun}.nc"
    cp $temp .
done

## this works ## now need to make an R script to extract hemis, and maybe make plots?? ##
