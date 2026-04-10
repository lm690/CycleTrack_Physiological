#!/bin/bash
#SBATCH --export=ALL
#SBATCH -p mrcq
#SBATCH --time=04:00:00
#SBATCH -A Research_Project-MRC190311
#SBATCH --nodes=1
#SBATCH --mail-type=END
#SBATCH --error=Garmindata_%j.err
#SBATCH --output=Garmindata_%j.log
#SBATCH --job-name=Garmindata

## Set project directory from submission location
export PROJECTDIR="${SLURM_SUBMIT_DIR}"

## Source config
source "${PROJECTDIR}/config.txt"

echo "PROJECTDIR=${PROJECTDIR}"
echo "SCRIPTSDIR=${SCRIPTSDIR}"
echo "PRODIR=${PRODIR}"
echo "RMDDIR=${RMDDIR}"

echo Job started on:
date -u

echo "Processing data located in: ${DATADIR}"
echo "Loading R module: ${RVERS}"
module load "${RVERS}"

mkdir -p "${R_LIBS_USER}" "${PANDOCHOME}"


## Bootstrap pandoc
if [ ! -x "${PANDOCDIR}/pandoc" ]; then
  echo "Pandoc not found, installing locally..."

  mkdir -p "${PANDOCHOME}"
  cd "${PANDOCHOME}" || exit 1

  wget -O pandoc.tar.gz \
    https://github.com/jgm/pandoc/releases/download/2.19.2/pandoc-2.19.2-linux-amd64.tar.gz

  tar -xzf pandoc.tar.gz

  mkdir -p "${PANDOCDIR}"
  cp pandoc-2.19.2/bin/pandoc "${PANDOCDIR}/pandoc"
  chmod +x "${PANDOCDIR}/pandoc"
fi

export PATH="${PANDOCDIR}:${PATH}"
export RSTUDIOPANDOC="${PANDOCDIR}"

echo "Pandoc location:"
which pandoc || true
pandoc --version || true

## Run scripts
Rscript "${SCRIPTSDIR}/00_Setup.R"
Rscript "${SCRIPTSDIR}/01_Load_Simple.R"
Rscript "${SCRIPTSDIR}/Render01_Loading.R"
Rscript "${SCRIPTSDIR}/02_Clean_Simple.R"
Rscript "${SCRIPTSDIR}/03_Daily_Derivation.R"
Rscript "${SCRIPTSDIR}/04_Adherence_BBI.R"
Rscript "${SCRIPTSDIR}/Render03_Daily_Summaries.R"
Rscript "${SCRIPTSDIR}/05_HRV_Simple.R"
Rscript "${SCRIPTSDIR}/06_Stress_Simple.R"
Rscript "${SCRIPTSDIR}/07_Steps_Simple.R"
Rscript "${SCRIPTSDIR}/08_HR_Simple.R"
Rscript "${SCRIPTSDIR}/09_Combine_Daily_Simple.R"

echo Job ended on:
date -u
