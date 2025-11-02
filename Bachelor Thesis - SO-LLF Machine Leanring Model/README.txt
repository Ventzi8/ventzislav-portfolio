Overview

This repository contains all supplementary code and data required to replicate the results presented in my thesis. 
The code implements simulations and empirical analyses related to Local Linear Forests (LLF) and Second-Order Local Linear Forests (SO-LLF), 
following and extending the methods in Friedberg et al. (2021). All scripts are provided for full transparency and reproducibility.

Strcuture:
├── Data/
│   ├── CPS Data.csv
│   └── Energy Appliance Data.csv
├── Figures and Tables/
│   ├── 5.2.1 - Simulations/
│   │   ├── 5.1.1 - Table 1.R
│   │   ├── 5.1.2 - Figure 2.R
│   │   └── 5.1.3 - Table 2.R
│   └── 5.2.2 - Empirical Studies/
│       ├── 5.2.1 - Table 3 and Figure 2/
│       │   ├── 5.2.1 - Figure 2.R
│       │   ├── 5.2.1 - Table 3 Lower Panel Benchmark models.R
│       │   ├── 5.2.1 - Table 3 Lower Panel SO-LLF Model.R
│       │   ├── 5.2.1 - Table 3 Upper Panel Benchmark Models.R
│       │   └── 5.2.1 - Table 3 Upper Panel SO-LLF.R
│       └── 5.2.2 - Table 4/
│           └── 5.2.2 - Table 4.R
└── Results/
    ├── 5.1.1 - Table 1/
    │   └── 5.1.1 - Table 1.csv
    ├── 5.1.3 - Table 2/
    │   ├── 5.1.3 - Table 2 Replication Results.csv
    │   └── 5.1.3 - Table 2.csv
    ├── 5.2.1/
    │   ├── 5.2.1 - Table 3 Lower Panel Benchmark.csv
    │   ├── 5.2.1 - Table 3 Lower Panel SO-LLF.csv
    │   ├── 5.2.1 - Table 3 Upper Panel Benchmark.csv
    │   └── 5.2.1 - Table 3 Upper Panel SO-LLF.csv
    └── 5.2.2 - Table 4/
        └── 5.2.2 - Table 4.csv

How to Use

    All scripts are independent and can be run separately.

    Scripts are written for R (tested on RStudio version 4.3.1).

    Required R packages include:

        - grf

        - glmnet

        - dbarts

        - xgboost

        - dplyr

        - readr

        - ggplot2

        - progress

        - (and any others used in the scripts)

    No additional setup beyond standard R dependencies is required.


Running the Code

    Data files (CPS Data.csv, Energy Appliance Data.csv) are located in the Data/ directory and are used for empirical analyses.

    Simulation and empirical scripts are organized under Figures and Tables/ by section. Each R script produces outputs relevant to tables/figures in the thesis.

    Results are written to the corresponding folder in the Results/ directory.

        Note: The provided CSV files are placeholders and are empty; actual outputs will be generated when you run the scripts.


Notes

    Each script is named according to the figure or table it produces for easy cross-referencing with the thesis.

    For exact reproduction, please use RStudio version 4.3.1 (or a compatible R version).

    The SO-LLF code is provided for research purposes and may require additional computational resources for large-scale experiments.