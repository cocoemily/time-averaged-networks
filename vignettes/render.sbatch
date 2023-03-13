#!/bin/bash
#
#SBATCH --job-name=ta-render
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=700GB
#SBATCH --time=24:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0
module load pandoc/2.11.3.2

R --no-save -q -f render.R
