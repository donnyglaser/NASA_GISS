#!/bin/bash

#SBATCH --ntasks-per-node=1

#SBATCH --time=12:00:00

#SBATCH --constraint=mil ## test this out to go to the less crowded servers??

runList=('TopoEns_0x0_LPaERSM40_a')
#runList+=('TopoEns_0x1_P2SNoM40_a')
#runList+=('TopoEns_0x5_P2SNoM40_a')
#runList+=('TopoEns_0x10_P2SNoM40_a')
#runList+=('TopoEns_0x20_P2SNoM40_a')
runList+=('TopoEns_0x30_P2SNoM40_a')
#runList+=('TopoEns_0x40_P2SNoM40_a')
#runList+=('TopoEns_0x50_P2SNoM40_a')
#runList+=('TopoEns_22x0_LPaERSM40_a')
#runList+=('TopoEns_22x1_P2SNoM40_a')
#runList+=('TopoEns_22x5_P2SNoM40_a')
#runList+=('TopoEns_22x10_P2SNoM40_a')
#runList+=('TopoEns_22x20_P2SNoM40_a')
#runList+=('TopoEns_22x30_P2SNoM40_a')
#runList+=('TopoEns_22x40_P2SNoM40_a')
#runList+=('TopoEns_22x50_P2SNoM40_a')
runList+=('TopoEns_90x0_LPaERSM40_a')
#runList+=('TopoEns_90x1_P2SNoM40_a')
#runList+=('TopoEns_90x5_P2SNoM40_a')
#runList+=('TopoEns_90x10_P2SNoM40_a')
#runList+=('TopoEns_90x20_P2SNoM40_a')
#runList+=('TopoEns_90x30_P2SNoM40_a')
runList+=('TopoEns_90x40_P2SNoM40_a')
runList+=('TopoEns_90x50_P2SNoM40_a')

for irun in ${runList[@]}
do
    cd $irun
    accList=(*.acc*.nc)
    if [${#accList[@]} > 1]
    then
        mkdir ACC
        mv *.acc*.nc ./ACC/
        cd ACC
        yearList=(DEC*)

        for iyear in ${yearList[@]}
        do
            sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}${iyear:3:4}.acc*
            scaleacc ANN${iyear:3:4}.acc* aij
        done

        annList=(ANN*.acc*)

        ed2=${#annList[@]}
        let ed2=ed2-1
        let ed1=ed2-50
        st=${annList[@]:$ed1:$ed2}

        sumfiles $st
        #sumfiles ${annList[@]}
        #echo "sum files"

        name=${annList[$ed1]:3:4}
        name+='-'
        name+=${annList[$ed2]:3:4}
        st=(*$name*)

        scaleacc $st aij

        echo $(date)

        mkdir ../ANN/
        mv ANN* ../ANN/
        cd ..
        cd ..
    fi
done

concatList=('TopoEns_0x20_P2SNoM40_a')
concatList+=('TopoEns_0x40_P2SNoM40_a')
concatList+=('TopoEns_0x50_P2SNoM40_a')
concatList+=('TopoEns_90x1_P2SNoM40_a')

#### 0x20 #### end=99
cd TopoEns_0x20_P2SNoM40_a
accList=(*.acc*.nc)
if [${#accList[@]} > 1]
then
    mkdir ACC
    mv *.acc*.nc ./ACC/
    cd ACC
    yearList=(DEC*)

    for iyear in {0..98}
    do
        sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}${yearList[iyear]:3:4}.acc*
        scaleacc ANN${yearList[iyear]:3:4}.acc* aij
    done

    annList=(ANN*.acc*)

    ed2=${#annList[@]}
    let ed2=ed2-1
    let ed1=ed2-50
    st=${annList[@]:$ed1:$ed2}

    sumfiles $st

    name=${annList[$ed1]:3:4}
    name+='-'
    name+=${annList[$ed2]:3:4}
    st=(*$name*)

    scaleacc $st aij

    echo $(date)

    mkdir ../ANN/
    mv ANN* ../ANN/
    cd ..
    cd ..
fi

cd TopoEns_0x20_P2SNoM40_b
accList=(*.acc*.nc)
if [${#accList[@]} > 1]
then
    mkdir ACC
    mv *.acc*.nc ./ACC/
    cd ACC
    yearList=(DEC*)

    for iyear in {0..98}
    do
        sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}${yearList[iyear]:3:4}.acc*
        scaleacc ANN${yearList[iyear]:3:4}.acc* aij
    done

    annList=(ANN*.acc*)

    ed2=${#annList[@]}
    let ed2=ed2-1
    let ed1=ed2-50
    st=${annList[@]:$ed1:$ed2}

    sumfiles $st

    name=${annList[$ed1]:3:4}
    name+='-'
    name+=${annList[$ed2]:3:4}
    st=(*$name*)

    scaleacc $st aij

    echo $(date)

    mkdir ../ANN/
    mv ANN* ../ANN/
    cd ..
    cd ..
fi
