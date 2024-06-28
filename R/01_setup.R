#script to setup project details

## load packages
library(tidyverse)
library(here)
## set options
options(mc.cores = parallel::detectCores())

## user setting
raft_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_mosquito_data.xlsx", Sys.info()[['user']])

pred_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_mosquito_predictions.xlsx", Sys.info()[['user']])

