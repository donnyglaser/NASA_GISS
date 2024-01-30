#!/bin/bash

##SBATCH --dependency=afterany:

#SBATCH --ntasks-per-node=1

#SBATCH --time=12:00:00

module load R/4.1.0

Rscript eqAnalysis.R

