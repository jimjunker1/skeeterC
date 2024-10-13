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
  data[["UTC_Date_&_Time (none)"]] <- as.POSIXct(data[["UTC_Date_&_Time (none)"]], format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")
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

#' @title tempFill_custom
#' @Description This function fills in temperature data that are above a specified threshold
#' @param df df is the full miniDot data.frame
#' @param too_hot This numeric variable specifies the temperature threshold above which the logger temp data is wrong
#'
tempFill_custom = function(df, too_hot = 36,...){
  tempVec = df$temp_C
  if(sum(tempVec[tempVec >= too_hot]) <= 4){
    cat("No temperatures over threshold")
    return(df)
  } else{
    firstHot = min(which(tempVec >= too_hot))
    lastHot = max(which(tempVec >= too_hot))
    risingModelDf = data.frame(
      time_seq = ((firstHot-30):firstHot) - firstHot,
      temp_C = tempVec[(firstHot - 30):firstHot]
    )
    risingModel = lm(temp_C ~ time_seq, data = risingModelDf)#;summary(risingModel)
    risingPred = data.frame(time_seq = (firstHot:lastHot) - firstHot,
                            temp_C = predict(risingModel, newdata = data.frame(time_seq = (firstHot:lastHot) - firstHot)))

    fallingModelDf = data.frame(
      time_seq = (lastHot:length(tempVec) - lastHot),
      temp_C = tempVec[(lastHot:length(tempVec))]
    )
    fallingModel = lm(temp_C ~ poly(time_seq,3), data = fallingModelDf)#;summary(fallingModel)
    fallingPred = data.frame(time_seq = (firstHot:lastHot) - lastHot,
                             temp_C = predict(fallingModel, newdata = data.frame(time_seq = (firstHot:lastHot) - lastHot)))

    upDownDf = data.frame(time_seq = firstHot:lastHot,
                          risingPred = risingPred$temp_C,
                          fallingPred = fallingPred$temp_C) %>%
      mutate(dist = abs(risingPred - fallingPred))

    minDist = upDownDf %>% slice_min(dist) %>% select(time_seq) %>% unlist

    temp_C_pred = ifelse(upDownDf$time_seq < minDist, upDownDf$risingPred, upDownDf$fallingPred)

    tempVec[firstHot:lastHot] <- temp_C_pred
    df$temp_C = tempVec
    return(df)
  }
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
