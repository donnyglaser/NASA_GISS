#!/bin/bash

#SBATCH --ntasks-per-node=11

module load R/4.1.0

Rscript eqAnalysis.R

