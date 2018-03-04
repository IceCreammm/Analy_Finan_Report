rm(list=ls())
library(DBI)
source("./CODE/LIB/lib_generic.R")
source("./CODE/LIB/lib_analy_finan_data.R")

###### import data from DB for further analysis ########
######## we analysis a single stock ID with defined 8 quarters

#################################### OVERALL settings ###################################
id_stock <- "002460"
date_report <- "2017-09-30"
num_qtr_back <- 8
#################################################################### OVERALL settings ###