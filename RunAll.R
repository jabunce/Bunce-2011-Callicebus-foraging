# clear the workspace
rm (list = ls(all=TRUE))

# Load packages
# if you don't have one or more of these packages, you can install them like this: install.packages(c("lattice", "MASS"))
library("lattice")
library("grid")
library("lme4")
library("Hmisc")
library("arm")
library("irr")

# Set working directory: the path to the folder containing the subfolders Code, Plots, and Data
# on mac
#setwd("/Users/johnbunce/Dropbox/John2/Callicebus_behavior_manuscript_github/foraging_analysis")
# on pc
setwd("C:/Users/jabunce/Dropbox/John2/Callicebus_behavior_manuscript_github/foraging_analysis")


# inter-observer reliability using intra-class correlation coeficients (ICCs)
# Open this file and run its contents in order to see the calculated ICCs.
source("./Code/R_InterObserv_ICC_22jan09.R")


# Makes Figure 1 in manuscript and puts it in the Plots folder
source("./Code/PatchCharacteristics_7oct10.R")


# Fit the multi-level Poisson regressions to individual-level foraging data.  
# Note that Poisson models are now fit with the "glmer" function in lme4 instead of the original "lmer", which no longer supports family=poisson
# As a result, model output is slightly different from the original paper, though qualitative results look similar.

# model comparison: deciding which predictors to include in subsequent analyses. Only the YOR patch models were used in the paper.
source("./Code/Patch_analysis_Rscript_10Feb09.R")

# more model comparison: take the two best-fitting models from the previous script and calculate confidence intervals
source("./Code/Patch_analysis_Rscript_28Feb09_CI.R")

# Use best-fitting model from the previous script to make Figure 2 (put in the Plots folder)
# Open this file and run its contents in order to see the values for Tables 1 and 2 in the paper.
source("./Code/Patch_analysis_Rscript_18apr11_CI_Pearson_Residplot_standardTest.R")

