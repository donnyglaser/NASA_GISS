#!/bin/bash

##SBATCH --dependency=afterany:

#SBATCH --ntasks-per-node=11

#SBATCH --time=12:00:00

module load R/4.1.0

allSTP=0

if ! test -f STOPFLAG_aij.rds; then
  Rscript eqAnalysis_aij.R
else
  ((allSTP++))
fi

if ! test -f STOPFLAG_aijk.rds; then
  Rscript eqAnalysis_aijk.R
else
  ((allSTP++))
fi

if ! test -f STOPFLAG_aijl.rds; then
  Rscript eqAnalysis_aijl.R
else
  ((allSTP++))
fi

if ! test -f STOPFLAG_oij.rds; then
  Rscript eqAnalysis_oij.R
else
  ((allSTP++))
fi

if ! test -f STOPFLAG_oijl.rds; then
  Rscript eqAnalysis_oijl.R
else
  ((allSTP++))
fi

if ((allSTP > 4)); then
  Rscript eqAnalysis_concat.R
fi
