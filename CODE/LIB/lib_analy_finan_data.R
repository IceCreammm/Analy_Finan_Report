####

########## the following function unzip the zip file obtained from "http://www.cninfo.com.cn/cninfo-new/index"
## which contains balance sheet, income statement and cash flow data 
prep_fin_data_from_zip <- function(inp_dir_zip_raw,
                                   inp_id_stock,
                                   inp_nam_type_report,
                                   inp_dict_data_imp, 
                                   inp_dir_unzip = paste0(inp_dir_zip_raw, "unzip")){
  ### inp_dir_zip_raw: directory contains the obtained zip files
  ### inp_id_stock: equity id, e.g., "002460", part of the zip file name
  ### inp_nam_type_report: type of report, e.g., "fzb", part of the zip file name, balance sheet
  stopifnot(is.character(inp_dir_zip_raw) & length(inp_dir_zip_raw) == 1)
  stopifnot(is.character(inp_id_stock) & length(inp_id_stock) == 1)
  stopifnot(is.character(inp_nam_type_report) & length(inp_nam_type_report) == 1)
  # A.1. unzip the zip file
  nam_zip <- list.files(
    inp_dir_zip_raw, 
    pattern = paste0(inp_nam_type_report, ".*", inp_id_stock, ".*", ".*.zip$")) 
  stopifnot(length(nam_zip) == 1) # must be a single zip file
  ###### unzip the files
  unzip(paste0(inp_dir_zip_raw, nam_zip), overwrite = TRUE, 
        exdir = inp_dir_unzip) #, 
  # A.2. get the list of unzipped CSV files
  nam_csv <- list.files(
    inp_dir_unzip, 
    pattern = paste0(inp_nam_type_report, ".*", inp_id_stock, ".*", ".*.csv$")) 
  stopifnot(length(nam_csv) > 0) ## at least there is one file
  # A.3. import CSV and collect them into df
  tmp_lst <- read_csv(paste0(inp_dir_unzip, "/", nam_csv[1]), col_names = FALSE, 
                      locale = locale(encoding = stringi::stri_enc_get())) %>% 
    clean_df_raw_by_dict(inp_dict_data_imp)
  df_bal_col <- tmp_lst$data
  meta_bal_col <- tmp_lst$meta
  ## A.3.0 in case more than 1 csv
  if (length(nam_csv) > 1) {
    for (idx in 2:length(nam_csv)) {
      tmp_lst <- read_csv(paste0(inp_dir_unzip, "/", nam_csv[idx]), col_names = FALSE, 
                          locale = locale(encoding = stringi::stri_enc_get())) %>% 
        clean_df_raw_by_dict(inp_dict_data_imp)
      df_bal_col <- bind_rows(df_bal_col, tmp_lst$data)
      meta_bal_col <-  union(meta_bal_col, tmp_lst$meta)
    }
  }
  tibble::lst(data = df_bal_col, meta = meta_bal_col)
}
