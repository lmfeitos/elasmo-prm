# 

This repository contains code used in the paper: Caughman, A.M., Gaines, S.D., Bradley, D. (2024) Climate change reduces long-term population benefits from no-take marine protected areas through selective pressures on species movement. _Global Change Biology_, 30(3), e17240.

The associated zenodo repository is located at https://doi.org/10.5281/zenodo.14928150 https://zenodo.org/badge/651985253.svg

For any questions, comments, or concerns, please contact Leonardo Feitosa [lmfeitosa@ucsb.edu](lmfeitosa@ucsb.edu) or Allie Caughman [acaughman@ucsb.edu](acaughman@ucsb.edu).

# Instructions

The order of running scripts should be as follows: 

1. The first script is the simulation model `01_fishbase_data_wrangling.Rmd`. This will produce a csv with the life history 
characteristics for modeling in the random forests. Pair this with the raw literature review data from Dryad.
2. The next two files you should run is the 02_random_forest_longline.Rmd and 03_random_forest_trawl_gillnet.Rmd. These
files train and run the random forest and produce figures associated with model validation, as well as the longline
prediction data.
3. the fourth script you should run is sim_prep.R. Input the model prediction data, and this script collects intrinsic growth rates
for each species from the FishLife package.
4. Next, run the retention prohibition simulation in 05_simulation.R. This script produces the resulting simulation dataset.
5. Lastly, run 06_plots.R. This file produces all manuscript figures from the random forest and simulation data sets.

# Repository Structure

## Overview

```
scripts
  |__ 01_fishbase_data_wrangling.Rmd
  |__ 02_random_forest_longline.Rmd
  |__ 03_random_forest_trawl_gillnet.Rmd
  |__ 04_sim_prep.R
  |__ 05_simulation.R
  |__ 06_plots.R
figs
  |__ supp
    |__ supplemental figures
  |__ fig1.pdf
  |__ fig2.pdf
  |__ fig3.pdf
  |__ fig4.pdf
  |__ fig5.pdf
```

# R Version

All code was run using R version 4.1.3

## Required Libraries

+ Data Ingestion, Cleaning, Harmonization, and Organization
  - `tidyverse` (version 1.3.2)
  - `here` (version 1.0.1)
  - `rfishbase` (version 4.1.2)
  - `FishLife` (version 3.1.0)
+ Random Forest Analysis
  - `tidymodels` (version 1.1.1) 
  - `ranger` (version 0.16.0) 
+ Data Visualization
  - `vip` (version 0.4.1)
  - `naniar` (version 1.0.0)
  - `patchwork` (version 1.1.2)
  - `ggpmisc` (version 0.5.5)
  - `ggridges` (version 0.5.6)
  - `ggh4x` (version 0.2.8)
