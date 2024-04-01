#!/bin/bash

#SBATCH --account=s1001

#SBATCH --nodes=1

#SBATCH --ntasks-per-node=44

#SBATCH --constraint="sky|cas"

#SBATCH --time=12:00:00

#SBATCH --output=./slurmOut/%x-%j-slurm.out

##SBATCH --dependency=afterany:

##################
RUNID=RUNID
##################

module purge

#module load nco/4.8.1

module load comp/intel/2021.3.0

module load mpi/impi/2021.3.0

#module load xxdiff/4.0.1

 

rm /discover/nobackup/projects/giss_ana/users/dmglaser/model_out/${RUNID}/lock

cd /home/dmglaser/modelE/exec

./runE ${RUNID} -np 44
