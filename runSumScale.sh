#!/bin/bash

## run this in the run's directory ##
## this script takes monthly output and sums to yearly, ##
## then scales the yearly to an aij and aijk file, ##
## then creates a sum file for the last 50 years of the simulation, ##
## lastly, this moves all acc and aij+aijk files to a ANN or ACC folder ##
## use this script to assess the timeseries equilibrium of a run ##

##SBATCH --ntasks-per-node=11

##SBATCH --time=12:00:00

##SBATCH --constraint=mil ## test this out to go to the less crowded servers??

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

name=${annList[$ed1]:3:4}
name+='-'
name+=${annList[$ed2]:3:4}
st=(*$name*)

scaleacc $st aij
scaleacc $st aijk

echo $(date)

mkdir ANN
mv ANN*.aij*.nc ./ANN/
mkdir ACC
mv *.acc*.nc ./ACC/