#!/bin/bash

#SBATCH --ntasks-per-node=11

#SBATCH --time=6:00:00

module load R/4.1.0

Rscript eqAnalysis_MarsPLD.R

