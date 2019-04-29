#!/bin/bash
#SBATCH -p debug
#SBATCH --job-name=MC-Ising
#SBATCH --output=run.out
#SBATCH --nodes=1
#SBATCH --time=6:00:00

srun -n 20 --mpi=pmi2  ./test
