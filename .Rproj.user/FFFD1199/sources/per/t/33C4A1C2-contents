rm(list=ls())
library(DBI)
source("./CODE/LIB/lib_generic.R")
source("./CODE/LIB/lib_analy_finan_data.R")

####### overall settings ########
dir_work <- "./"
dir_data_raw <- paste0("D:/Project_R_Py/analyze_fin_report/", "DATA/RAW/")
nam_prime_key <- c("F001D", "SECCODE") ### portfolio date & stock code
###################################


### import data dictionary ###
dict_bal <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "bal_sheet")
dict_pnl <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "income_stat")
dict_cash <- readxl::read_excel(paste0(dir_work, "DATA/", "dict_import_csv_R.xlsx"), sheet = "cash_flow")
################################


############################## BEGIN A: Collect all balance sheet ######################
nam_type_report <- "fzb"
### get all stock id avaiables
nam_id_stocks <- list.files(dir_data_raw, 
                            pattern = paste0(nam_type_report, ".*.zip$")) %>% 
  stringr::str_sub(start = 8, end = 13) %>% 
  unique()
lst_bal <- map(nam_id_stocks, function(x) prep_fin_data_from_zip(
  dir_data_raw, x, nam_type_report, dict_bal)) %>% 
  col_data_meta_from_lst_of_lst()
### final clean & prepare for output
stopifnot(NROW(lst_bal$data) == NROW(distinct(select(lst_bal$data, nam_prime_key))))
lst_bal$meta <- lst_bal$meta %>% 
  mutate(prime_key = if_else(varR %in% nam_prime_key, TRUE, FALSE)) %>% 
  arrange(desc(prime_key), varR)
################################################## END A: Collect all balance sheet ##



############################## BEGIN B: Collect all income statement ######################
nam_type_report <- "lrb"
### get all stock id avaiables
nam_id_stocks <- list.files(dir_data_raw, 
                            pattern = paste0(nam_type_report, ".*.zip$")) %>% 
  stringr::str_sub(start = 8, end = 13) %>% 
  unique()
lst_pnl <- map(nam_id_stocks, function(x) prep_fin_data_from_zip(
  dir_data_raw, x, nam_type_report, dict_pnl)) %>% 
  col_data_meta_from_lst_of_lst()
### final clean & prepare for output
stopifnot(NROW(lst_pnl$data) == NROW(distinct(select(lst_pnl$data, nam_prime_key))))
lst_pnl$meta <- lst_pnl$meta %>% 
  mutate(prime_key = if_else(varR %in% nam_prime_key, TRUE, FALSE)) %>% 
  arrange(desc(prime_key), varR)
################################################## END B: Collect all balance sheet ##



############################## BEGIN C: Collect cash flow statement ######################
nam_type_report <- "llb"
### get all stock id avaiables
nam_id_stocks <- list.files(dir_data_raw, 
                            pattern = paste0(nam_type_report, ".*.zip$")) %>% 
  stringr::str_sub(start = 8, end = 13) %>% 
  unique()
lst_cash <- map(nam_id_stocks, function(x) prep_fin_data_from_zip(
  dir_data_raw, x, nam_type_report, dict_cash)) %>% 
  col_data_meta_from_lst_of_lst()
### final clean & prepare for output
stopifnot(NROW(lst_cash$data) == NROW(distinct(select(lst_cash$data, nam_prime_key))))
lst_cash$meta <- lst_cash$meta %>% 
  mutate(prime_key = if_else(varR %in% nam_prime_key, TRUE, FALSE)) %>% 
  arrange(desc(prime_key), varR)
################################################## END C: Collect all balance sheet ##








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

## write balance sheets into db
dbWriteTable(conn_db, "tabl_ts_bal_sheet", lst_bal$data)
dbWriteTable(conn_db, "meta_ts_bal_sheet", lst_bal$meta)

## write income statement
dbWriteTable(conn_db, "tabl_ts_income_stat", lst_pnl$data)
dbWriteTable(conn_db, "meta_ts_income_stat", lst_pnl$meta)

## write cash flow statement
dbWriteTable(conn_db, "tabl_ts_cash_flow", lst_cash$data)
dbWriteTable(conn_db, "meta_ts_cash_flow", lst_cash$meta)


## checks
dbListTables(conn_db)
## disconnect
dbDisconnect(conn_db)
################################################## END D: Write into SQLite ##







################### save data ##################
### 
# save(nam_prime_key, dict_bal, dict_cash, dict_pnl, lst_bal, lst_cash, lst_pnl,
#      file = paste0(dir_work, "DATA/CLEAN/", "COLLECT_CSV_RAW.Rdata"))
##############################




