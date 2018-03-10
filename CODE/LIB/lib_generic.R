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
  ## return results
  select(inp_df, one_of(setdiff(names(inp_df), nam_var_num))) %>% 
    bind_cols(select(inp_df, one_of(nam_var_num)) - select(inp_df_lag, one_of(nam_var_num))) %>% 
    select(names(inp_df))
}