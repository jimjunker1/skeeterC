#script to setup project details

## load packages
library(tidyverse)

## set options
options(mc.cores = parallel::detectCores())

## user setting
raft_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_mosquito_data.xlsx", Sys.info()[['user']])

