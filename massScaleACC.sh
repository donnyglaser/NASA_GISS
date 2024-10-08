#!/bin/bash

diags=()
## Remove comment for the desired diagnostics ##

# ATMOS #
diags+=("aij")
#diags+=("aijk")
#diags+=('aijl')
#diags+=('taij')
#diags+=('taijl')

# OCEAN #
#diags+=('oij')
#diags+=('oijk')
#diags+=('oijl')
#diags+=('toij')
#diags+=('toijl')

## SUBDD ##
#diags+=('aijph2')
#diags+=('taijph2')
#diags+=('oijph2')
#diags+=('toijph2')

# list all .acc files
accAll=`ls ./*.acc*`
runName=`ls ./JAN*.acc*.nc`
x=${runName[0]:13:1}

for ifile in $accAll ## this isnt working like I thought
do
    for idiag in "${diags[@]}"
    do
        scaleacc $ifile $idiag
    done
done

## makes a directory and moves the files
mkdir diagOut

for i in "${diags[@]}"
do
        target="*."$i$x"*"
        mv -f $target ./diagOut/
done

## Remove Partial Files ##
cd diagOut
rm PARTIAL.*