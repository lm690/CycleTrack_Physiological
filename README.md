## Setup
Project directory: This will contain the .sh file and the config.txt file.
data directory: This will need a subdirectory "1_raw" with the participant data contained within the download folders (e.g. cbac22d020d1f88ee22f938f6fdbcf00).
Scripts directory: This is where all the R and Rmd scripts will be pulled from. Subdirectory of Project directory.

config.txt file: Exports directories that can be called in R scripts. Uses R/4.2.1-foss-2022a.

## Run
Set working directory to Project directory. sbatch 1_runGarmin_DataPrep.sh.

## To Do
Set script to pull data from AWS bucket (might require conda set up).
Clean scripts.
Create Rmd outputs for cleaned daily summaries.
Create actigraphy summaries.
