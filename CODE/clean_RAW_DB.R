rm(list=ls())
library(DBI)
source("./CODE/LIB/lib_generic.R")
source("./CODE/LIB/lib_analy_finan_data.R")

####### overall settings ########
dir_work <- "./"
dir_db_raw <- paste0(dir_work, "DATA/CLEAN/", "db_api_report.sqlite")
# nam_prime_key <- c("F001D", "SECCODE") ### portfolio date & stock code
nam_tabl_bal <- "tabl_RAW_BAL_API"
nam_tabl_pnl <- "tabl_RAW_PNL_API"
nam_tabl_cash <- "tabl_RAW_CASH_API"
nam_tabl_indi <- "tabl_RAW_INDI_API"
###################################

### import data dictionary ###
dict_bal <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "bal_sheet")
dict_pnl <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "income_stat")
dict_cash <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "cash_flow")
dict_indi <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "key_indi")
################################

### set up connection
conn_db_api <- dbConnect(RSQLite::SQLite(), dir_db_raw)
dbListTables(conn_db_api)






################################################################################
################################################################################
################################################################################
############################## BEGIN D: Write into SQLite ######################
## set up conn
conn_db <- dbConnect(RSQLite::SQLite(), paste0(dir_work, "/DATA/CLEAN/", "db_finan_report.sqlite"))
## write dict into db
dbWriteTable(conn_db, "dict_bal_sheet", dict_bal)
dbWriteTable(conn_db, "dict_income_stat", dict_pnl)
dbWriteTable(conn_db, "dict_cash_flow", dict_cash)
dbWriteTable(conn_db, "dict_key_indi", dict_indi)

## write balance sheets into db
lst_data <- clean_db_tabl_cninfo_api(conn_db_api, nam_tabl_bal, dict_bal)
dbWriteTable(conn_db, "tabl_ts_bal_sheet", lst_data$data)
dbWriteTable(conn_db, "meta_ts_bal_sheet", lst_data$meta)

## write income statement
lst_data <- clean_db_tabl_cninfo_api(conn_db_api, nam_tabl_pnl, dict_pnl)
dbWriteTable(conn_db, "tabl_ts_income_stat", lst_data$data)
dbWriteTable(conn_db, "meta_ts_income_stat", lst_data$meta)

## write cash flow statement
lst_data <- clean_db_tabl_cninfo_api(conn_db_api, nam_tabl_cash, dict_cash)
dbWriteTable(conn_db, "tabl_ts_cash_flow", lst_data$data)
dbWriteTable(conn_db, "meta_ts_cash_flow", lst_data$meta)

## write cash Key indicator
lst_data <- clean_db_tabl_cninfo_api(conn_db_api, nam_tabl_indi, dict_indi)
dbWriteTable(conn_db, "tabl_ts_key_indi", lst_data$data)
dbWriteTable(conn_db, "meta_ts_key_indi", lst_data$meta)




## checks
dbListTables(conn_db)
## disconnect
dbDisconnect(conn_db)
dbDisconnect(conn_db_api)
################################################## END D: Write into SQLite ##





