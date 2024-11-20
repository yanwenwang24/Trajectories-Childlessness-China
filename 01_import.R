## ------------------------------------------------------------------------
##
## Script name: 01_import.R
## Purpose: Import libraries and data
## Author: Yanwen Wang
## Date Created: 2024-11-20
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(haven)
library(janitor)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(ggseqplot)
library(nnet)
library(ggeffects)
library(patchwork)
library(stargazer)

select <- dplyr::select

# Set theme
theme_set(theme_classic())

# Import function
source("functions.R")

# Import data
load("Datasets/seq_marital_1540.RData")

# Download and import this file from the official CFPS website
cfps2018_familyroster <- read_stata("Datasets/cfps2018_familyroster.dta") %>%
  mutate_all(funs(replace(., . < 0, NA)))
