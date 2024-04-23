#!/bin/bash

#SBATCH --ntasks-per-node=1

#SBATCH --time=12:00:00


# run in dmglaser/model_out folder
fileList=(TopoEns_*)

for ifile in ${fileList[@]}
do
    cd $ifile
    mkdir ACC
    mv *.acc* ./ACC/
    cd ACC
    yearList=(DEC*)

    for iyear in ${yearList[@]}
    do
        sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}${iyear:3:4}.acc${ifile##*acc}
        scaleacc ANN${iyear:3:4}* aij
    done

    ed2=${#yearList[@]}
    let ed2=ed2-1
    let ed1=ed2-50
    st=${yearList[@]:$ed1:$ed2}

    name=${yearList[$ed1]:3:4}
    name+='-'
    name+=${yearList[$ed2]:3:4}
    sumfiles $st
    st=(*$name*)
    scaleacc $st aij


    mkdir ../ANN/
    mv ANN* ../ANN/
    cd ..
done



