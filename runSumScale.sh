#!/bin/bash

##SBATCH --ntasks-per-node=11

##SBATCH --time=12:00:00

##SBATCH --constraint=mil ## test this out to go to the less crowded servers??

## run in the run's directory
mkdir ACC
mv *.acc*.nc ./ACC/
cd ACC

echo $(date)
yearList=(DEC*)

for iyear in ${yearList[@]}
do
    sumfiles {JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC}${iyear:3:4}.acc*  #${ifile##*acc}
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