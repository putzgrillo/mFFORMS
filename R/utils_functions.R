# create matrix for meta_learner
create_classification_table <- function(.features, .accuracy) {
  # filter best combination
  bst_comb <- .accuracy %>%
    dplyr::arrange(MASE) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.model)
  
  # create table
  tibble(
    best_method = bst_comb,
    .features
  )
}

# adjust tsibble to ts if hourly data
as_ts <- function(.tsibble) {
  
  # get position an class of the tsibble index
  index_var <- index(.tsibble)
  index_posix <- .tsibble %>% 
    pull(index_var) %>%
    is.POSIXct()
  
  # if the index is posix, return default as.ts, 
  if( !index_posix ) {
    return(as.ts(.tsibble))
  }
  
  # if is posix
  ## the date of the first observation
  first_obs_date <- min(.tsibble[[index_var]]) %>%      
    lubridate::as_date() %>%
    as.numeric()
  
  ## the hour of the first observation
  first_obs_hour <- min(.tsibble[[index_var]]) %>%
    lubridate::hour()
  
  ## return the ts with 
   ts(
    data = .tsibble[["value"]],
    frequency = 24,
    start = c(first_obs_date, first_obs_hour)
  )
}

# create matrix ts
as_mts <- function(y_mts, x_date, freq) {
  y_mts <- as.data.frame(y_mts)
  
  mts <- lapply(y_mts, function(x) {
    ts(x, frequency = freq, start = min(x_date), end = max(x_date))
  }) |> do.call(what = cbind)
  
  return(mts)
}
