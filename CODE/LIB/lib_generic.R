####
 library(tidyverse)


## this function convert string into cap letters after removing spaces 
toUpperNoSpace <- function(stringInput){ return(toupperNoSpace(stringInput)) }
toupperNoSpace <- function(stringInput){
  stopifnot(is.character(stringInput))
  return(gsub(" ", "", toupper(stringr::str_trim(stringInput))) )
}
 
 
## this function prepare RAW df into lst with data and meta according to input dict
 clean_df_raw_by_dict <- function(inp_df_raw_data, inp_df_dict){
  ## we assume 1st ROW of inp_df_raw_data contains column names of clean df
  stopifnot(all(c("varR", "varXls") %in% names(inp_df_dict)))
  stopifnot(all(!duplicated(inp_df_dict$varXls)))
  stopifnot(all(!(is.na(inp_df_dict$varXls))))
  stopifnot(is.character(inp_df_dict$varXls))
  ## get raw data column names
  # if NULL (default) the 1st row of inp_df_raw_data, otherwise names(df...)
  nam_col_df <- as.character(inp_df_raw_data[1, ])
  stopifnot(is.character(nam_col_df))
  nam_varXls_comm <- intersect(nam_col_df, inp_df_dict$varXls)
  stopifnot(length(nam_varXls_comm) > 0)
  df_raw_data <- slice(inp_df_raw_data, -1) %>% 
    select(match(nam_varXls_comm, nam_col_df))
  names(df_raw_data) <- inp_df_dict$varR[match(nam_varXls_comm, inp_df_dict$varXls)]
  df_dict <- filter(inp_df_dict, varXls %in% nam_varXls_comm)
  ## data transformation is type is avaiable in the dictionary
  if ("type" %in% names(df_dict)) {
    nam_num <- df_dict$varR[toupperNoSpace(df_dict$type) == "NUM"]
    nam_char <- df_dict$varR[toupperNoSpace(df_dict$type) %in% c("STR", "CHAR")]
    nam_date <- df_dict$varR[toupperNoSpace(df_dict$type) == "date"]
    df_raw_data <- df_raw_data %>% 
      mutate_at(nam_num, function(x) as.numeric(stringr::str_trim(x))) %>% 
      mutate_at(nam_char, function(x) stringr::str_trim(x)) %>% 
      mutate_at(nam_date, function(x) as.Date(stringr::str_trim(x)))
  }
  tibble::lst(data=df_raw_data, meta=df_dict)
}


## collect lst of list with data and meta df
col_data_meta_from_lst_of_lst <- function(inp_lst_of_lst_data_meta){
  len_lst <- length(inp_lst_of_lst_data_meta)
  stopifnot(len_lst > 0)
  ## check each element muct contains data and meta
  walk(seq_len(len_lst), 
       function(x) stopifnot(all(c("data", "meta") %in% names(inp_lst_of_lst_data_meta[[x]]))))
  # get 1st element
  lst_res <- lst(data = inp_lst_of_lst_data_meta[[1]]$data, meta = inp_lst_of_lst_data_meta[[1]]$meta)
  # keep appending 
  if (len_lst > 1){
    for (idx in 2:len_lst){
      lst_res$data = bind_rows(lst_res$data, inp_lst_of_lst_data_meta[[idx]]$data)
      lst_res$meta = union(lst_res$meta, inp_lst_of_lst_data_meta[[idx]]$meta)
    }
  }
  lst_res
}

## this function clean RAW db data sourced from cninfo API directly
clean_db_tabl_cninfo_api <- function(inp_conn, inp_nam_tabl, inp_dict){
  nam_tabl_db <- dbListFields(inp_conn, inp_nam_tabl)
  ## checks
  stopifnot(c("varAPI", "varR") %in% names(inp_dict))
  nam_var_tabl_consolid <- with(inp_dict, varAPI[varR == "F002V"])
  stopifnot(c("index", nam_var_tabl_consolid) %in% nam_tabl_db)
  stopifnot(identical("index", setdiff(nam_tabl_db, inp_dict$varAPI)))
  nam_tabl_db_2_change <- nam_tabl_db[nam_tabl_db != 'index']
  nam_tabl_varR <- with(inp_dict, varR[match(nam_tabl_db_2_change, varAPI)])
  stopifnot(c("F001D", "SECCODE") %in% nam_tabl_varR)
  #nam_varR_num <- intersect(nam_tabl_varR, with(inp_dict, varR[toupperNoSpace(type) == "NUM"]))
  #nam_varR_date <- intersect(nam_tabl_varR, with(inp_dict, varR[toupperNoSpace(type) == "DATE"]))
  ## select data and prepare
  df_tabl <- dbGetQuery(inp_conn, paste0(
    "SELECT * FROM ", inp_nam_tabl, " WHERE ", nam_var_tabl_consolid, " = '",'071001', "'")) %>% 
    as_tibble() %>% 
    select(-index) %>% 
    rename_(.dots = set_names(nam_tabl_db_2_change, nam_tabl_varR)) %>% 
    select(F001D, SECCODE, everything()) 
  stopifnot(NROW(df_tabl) > 0)
  if (NROW(df_tabl) != NROW(distinct(select(df_tabl, F001D, SECCODE)))){
    df_tabl <- filter(df_tabl, F001D == ENDDATE)
  }
  stopifnot((NROW(df_tabl) == NROW(distinct(select(df_tabl, F001D, SECCODE)))))
  meta <- filter(inp_dict, varR %in% names(df_tabl)) %>% 
    mutate(prime_key = varR %in% c("F001D", "SECCODE")) %>% 
    arrange(desc(prime_key))
  lst(data = df_tabl, meta = meta)
}


## this function extrate increment from cumulative df. 
## e.g., from cumulative Balalce sheets get single qtr data
get_increment_from_cumulate <- function(inp_df, inp_nam_var_date){
  stopifnot(inp_nam_var_date %in% names(inp_df))
  ## Step 1: arrange inp_df according to given date 
  inp_df <- inp_df %>% 
    arrange_(.dots = inp_nam_var_date)
  ## Step 2: inpur df names that are numeric variables
  nam_var_num <- names(inp_df)[purrr::map_lgl(
    names(inp_df), function(x) is.numeric(pull(inp_df, x)))] 
  ## Step 3: prepare df with lag values, 
  inp_df_lag <- inp_df %>% 
    mutate_at(nam_var_num, lag) ## since the dates are sorted Step 1
  inp_df_lag[1, match(nam_var_num, names(inp_df_lag))] <- 0
  ## return results
  select(inp_df, one_of(setdiff(names(inp_df), nam_var_num))) %>% 
    bind_cols(select(inp_df, one_of(nam_var_num)) - select(inp_df_lag, one_of(nam_var_num))) %>% 
    select(names(inp_df))
}


## this function mutate existing df according to new_var_dictionary
get_new_var_2_df <- function(inp_df_data, inp_df_meta, inp_dict_new_var){
  ## checks
  stopifnot(c("varR") %in% names(inp_df_meta))
  stopifnot(c("varR", "formula") %in% names(inp_dict_new_var))
  stopifnot(identical(sort(names(inp_df_data)), sort(inp_df_meta$varR)))
  ## extend new data field
  df_data <- inp_df_data
  for (idx in seq_len(length(inp_dict_new_var$varR))) {
    df_data <- mutate_(df_data, .dots = set_names(inp_dict_new_var$formula[idx], inp_dict_new_var$varR[idx]))
  }
  nam_varR_comm <- intersect(inp_df_meta$varR, inp_dict_new_var$varR)
  df_meta <- bind_rows(filter(inp_df_meta, !(varR %in% nam_varR_comm)), inp_dict_new_var)
  stopifnot(identical(sort(names(df_data)), sort(df_meta$varR)))
  lst(data = df_data, meta = df_meta)
}



## this function plot and tab the equity ts from db
get_plot_tab_equity_ts <- function(inp_df_ts, inp_nam_var_date, inp_nam_var_2_plot, inp_dict_label, 
                                   inp_date_qtr_back = NULL, inp_type_ratio = FALSE) {
  ## checks
  stopifnot(all(c(inp_nam_var_date, inp_nam_var_2_plot) %in% names(inp_df_ts)))
  ## prepare data for plot
  df_res <- select(inp_df_ts, one_of(inp_nam_var_date, inp_nam_var_2_plot)) %>% 
    select(one_of(inp_nam_var_date, inp_nam_var_2_plot)) %>% 
    arrange_(.dots = inp_nam_var_date) %>% 
    cal_QoQ_YoY_of_df(inp_nam_var_2_plot) 
  if (!is.null(inp_date_qtr_back)) {
    df_res <- filter_(df_res, .dots = paste0(inp_nam_var_date, " %in% inp_date_qtr_back"))
  }
  plot_df_val_QoQ_YoY(df_res, inp_dict_label, inp_nam_var_date, inp_type_ratio = inp_type_ratio)
}

## this function calculates QoQ and YoY change of variables nam_var_2_cal from dataframe inp_df 
## Note that inp_df is assumed sorted according time (from history to current)
cal_QoQ_YoY_of_df <- function(inp_df, nam_var_2_cal) {
  stopifnot(all(nam_var_2_cal %in% names(inp_df)))
  nam_var_2_gather <- names(inp_df)[!(names(inp_df) %in% nam_var_2_cal)]
  df_qoq <- select(inp_df, one_of(nam_var_2_cal)) /
    mutate_all(select(inp_df, one_of(nam_var_2_cal)), lag) -1
  df_qoq <- rename_(df_qoq, .dots = set_names(nam_var_2_cal, paste0(nam_var_2_cal, "_QoQ")))
  df_yoy <- select(inp_df, one_of(nam_var_2_cal)) /
    mutate_all(select(inp_df, one_of(nam_var_2_cal)), function(x) lag(x, n=4)) - 1
  df_yoy <- rename_(df_yoy, .dots = set_names(nam_var_2_cal, paste0(nam_var_2_cal, "_YoY")))
  bind_cols(inp_df, df_yoy, df_qoq) %>% 
    gather(key = "measure", value = "val", -one_of(nam_var_2_gather)) %>% 
    mutate(type = if_else(grepl("._YoY$", measure), "YoY", 
                          if_else(grepl("._QoQ$", measure), "QoQ", "value"))) %>% 
    mutate(var = if_else(type == "QoQ", gsub("_QoQ", "", measure), 
                         if_else(type == "YoY", gsub("_YoY", "", measure), measure))) %>% 
    select(-measure) %>% 
    spread(key = "type", value = "val") %>% 
    arrange_(.dots = c("var", nam_var_2_gather)) %>% 
    select(one_of(nam_var_2_gather), var, value, YoY, QoQ)
}




## this fucntion prodce ggplot and table for output of cal_QoQ_YoY_of_df function
plot_df_val_QoQ_YoY <- function(inp_df_val_QoQ_YoY, inp_dict_label, inp_nam_var_date, inp_type_ratio = FALSE){
  stopifnot(all(c(inp_nam_var_date, "var", "value", "YoY", "QoQ") %in% names(inp_df_val_QoQ_YoY)))
  stopifnot(all(c("varR", "label") %in% names(inp_dict_label)))
  ## ggplot object
  if (inp_type_ratio) {
    inp_df_val_QoQ_YoY <- mutate(inp_df_val_QoQ_YoY, value = round(value, 4))
  } else {
    inp_df_val_QoQ_YoY <- mutate(inp_df_val_QoQ_YoY, value = round(value/1e6, 2))
  }
  obj_ggplot <- inp_df_val_QoQ_YoY %>%
    mutate_at(inp_nam_var_date, as.Date) %>% 
    left_join(select(inp_dict_label, varR, label), by=c("var" = "varR")) %>% 
    ggplot(aes_string(x = inp_nam_var_date, y = "value")) +
    geom_line(color='steelblue') +
    geom_point(size = 1.5) +
    #geom_bar(stat="identity", fill = "darkblue") +
    facet_wrap(~label, ncol = 1, scales="free_y") +
    xlab("") 
  if (!inp_type_ratio) {
    obj_ggplot <- obj_ggplot + ylab("Value, mln") + scale_y_continuous(label=scales::comma)
  } else{
    obj_ggplot <- obj_ggplot + ylab("%") + scale_y_continuous(label=scales::percent)
  }
  ## reshape table to view time series
  df_res <- gather(inp_df_val_QoQ_YoY, type, value, -one_of(inp_nam_var_date, "var")) %>% 
    mutate(measure = if_else(type == "value", var, paste0(var, "_", type)),
           val = if_else(type == "value", scales::comma(round(value, 2)), 
                         paste0(round(value*100), "%"))) %>% ### !!! PRONE to ERROR HERE!!! QUICK FIX
    select(one_of(inp_nam_var_date), measure, val) %>% 
    spread(key = measure, value = val) %>% 
    (function(x) as_tibble(cbind(.var = names(x), t(x)))) %>% 
    (function(x) rename_(x, .dots = set_names(names(x)[-1], as.character(x[1, -1])))) %>% 
    slice(-1) %>% 
    left_join(select(inp_dict_label, varR, label), by=c(".var" = "varR")) %>% 
    mutate(measure = if_else(is.na(label), .var, label)) %>% 
    select(-label, -.var) %>% 
    select(measure, everything())
  lst(obj_ggplot = obj_ggplot, df_view_ts = df_res)
}