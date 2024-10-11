here::i_am("code/01_setup.R")
#script to setup project details

## load packages
library(tidyverse)
library(here)

## define user functions
#' @title read_miniDOT
#' @Description This function reads in data from the output of PME miniDOT logger software
#' @param x x is the filepath to the TXT file exported from miniDOTconcatenate program
#'
read_miniDOT = function(x, list = FALSE,...){
  metadata = readLines(x, n = 6)
  labels = trimws(read.table(x, header = FALSE, sep = ",", nrows = 1, skip = 7))
  units = trimws(read.table(x, header = FALSE, sep = ",", nrows = 1, skip = 8))
  data = read.table(x, header = FALSE, sep = ",", skip = 9, strip.white = TRUE)
  colNames = paste(labels, units)
  colnames(data) <- colNames
  data[["UTC_Date_&_Time (none)"]] <- as.POSIXct(data[["UTC_Date_&_Time (none)"]], format = "%Y-%m-%d %H:%M:%S")
  data[["Central Standard Time (none)"]] <- as.POSIXct(data[["Central Standard Time (none)"]], format = "%Y-%m-%d %H:%M:%S")
  if(list){
    return(list(metadata = metadata,
                data = data))
  } else{return(data)}
}

#' Split a table in into parts of balanced size
#'
#' Splits a table into a  `n_tb` parts of similar sizing using standard styling in  `my_kable()`. This is
#'   used when I want to split a long thin table across multiple columns to reduce a document's size. Returns
#'   a single table part. Which part given by argument  `index`.
#'
#' Printing all table parts in one function call didn't work since I couldn't get
#'    `kable()` to print from within a function.
#'
#' @param df A data frame.
#' @param index Which part of the split table to return.
#' @param n_tb The number of parts to split the table into.
#' @param digits Sets the number of digits (via the  `kable()`  `digits` argument).
#'
#' @export
split_kable <- function(df, index, n_tb = 2, digits = 3,...) {
  nn <- dim(df)[1]
  end <- 0
  for (i in 1 : index){
    start <- end + 1
    rem <- nn - start + 1
    end <- start + ceiling(rem / (n_tb - i + 1)) - 1
  }
  tb <- dplyr::slice(df, start : end, .preserve=TRUE)
  return(kable(tb, digits = digits,...))
}
## set options
options(mc.cores = parallel::detectCores())
theme_set(theme_minimal())
## user setting
raft_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_mosquito_data.xlsx", Sys.info()[['user']])

pred_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/Shared Documents - Leaf Litter Hot Moments/General/2024_predictions_aeri_seed_mosquito.xlsx", Sys.info()[['user']])

metab_meta_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_logger-meta.xlsx", Sys.info()[['user']])
metab_depth_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_water-depth_data.xlsx", Sys.info()[['user']])
metab_folderpath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/DO logger data/", Sys.info()[['user']])
