#!/bin/bash

## modified from runSumScale.sh to take input of annual files, rather than monthlies ##
## run this in the run's directory ##
## this script scales the yearly to an aij and aijk file, ##
## then creates a sum file for the last 50 years of the simulation, ##
## lastly, this moves all acc and aij+aijk files to a ANN or ACC folder ##
## use this script to assess the timeseries equilibrium of a run ##

##SBATCH --ntasks-per-node=11

##SBATCH --time=12:00:00

##SBATCH --constraint=mil ## test this out to go to the less crowded servers??

echo $(date)
yearList=(ANN*)

for iyear in ${yearList[@]}
do
    scaleacc $iyear aij
done

annList=(ANN*.acc*)

ed2=${#yearList[@]}
let ed2=ed2-1
let ed1=ed2-50
st=${yearList[@]:$ed1:$ed2}

sumfiles $st

name=${yearList[$ed1]:3:4}
name+='-'
name+=${yearList[$ed2]:3:4}
st=(*$name*)

scaleacc $st aij
scaleacc $st aijk

echo $(date)

mkdir ANN
mv ANN*.aij*.nc ./ANN/
mkdir ACC
mv *.acc*.nc ./ACC/