#!/bin/bash

#SBATCH --mail-user=francesco.galli@vetsuisse.unibe.ch
#SBATCH --mail-type=begin,end,fail
#SBATCH --job-name="make_links"

#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=00:40:00
#SBATCH --array=1-2185:14


#SBATCH --output=/storage/homefs/fg19b682/create_transport_links/output_files/output_%j.o
#SBATCH --error=/storage/homefs/fg19b682/create_transport_links/output_files/error_%j.e

module load R

R CMD BATCH --no-save --no-restore create_links_array.R script_${SLURM_ARRAY_TASK_ID}.R
