#!/bin/bash
#SBATCH --job-name=kano_metrics
#SBATCH --output=output_kano.log
#SBATCH --error=error_kano.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=7
#SBATCH --time=24:00:00
#SBATCH --mem=16G
#SBATCH --mail-type=END
#SBATCH --mail-user=lmhlanga@luc.edu  # ← Replace this with your actual email

echo "Job started on $(date)"

# Start timing
start=$(date +%s)

module load R

Rscript abidjan_footprint_parallel.R

# End timing
end=$(date +%s)
runtime=$((end-start))
echo "Job completed on $(date)"
echo "Total runtime: $runtime seconds (~ $((runtime / 60)) minutes)"
