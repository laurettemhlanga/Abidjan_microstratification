#!/bin/bash
#SBATCH --job-name=abidjan_metrics
#SBATCH --output=output_abidjan.log
#SBATCH --error=error_abidjan.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=7
#SBATCH --time=48:00:00
#SBATCH --mem=16G
#SBATCH --mail-type=END
#SBATCH --mail-user=lmhlanga@luc.edu

echo "Job started on $(date)"
echo "Running on node: $(hostname)"
echo "Current directory: $(pwd)"

# Start timing
start=$(date +%s)

# Load R module
module load R

# Run the R script (in the same directory as this .sh file)
Rscript --vanilla abidjan_nnindex.R

# End timing
end=$(date +%s)
runtime=$((end-start))

echo "Job completed on $(date)"
echo "Total runtime: $runtime seconds (~ $((runtime / 60)) minutes)"

