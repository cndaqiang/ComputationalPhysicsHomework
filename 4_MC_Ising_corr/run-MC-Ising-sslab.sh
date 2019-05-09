#!/bin/bash
#
#SBATCH --job-name=MC
#SBATCH --output=MC.out
#SBATCH -N 1 
#SBATCH --ntasks-per-node=20
#SBATCH --time=24:00:00
#SBATCH -p regular


srun --mpi=pmi2  ./test
