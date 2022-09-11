#' Calculate time series forecast ----
#'
#' @param .y                ts: a time series object
#' @param scaled            logical: should the ts be scaled to compute the features?
#'
#' @return a tibble with time series features
#'
#' @export
ts_features <- function(.y, scaled = TRUE) {
  # adjust if is passed as tsibble
  if (tsibble::is_tsibble(.y)) {
    .y <- .y %>% as.ts()
  }
  # calculate set of features 
  ts_features <- tsfeatures::tsfeatures(
    .y,
    features = c("acf_features", "arch_stat", 
                 "crossing_points", "entropy", 
                 "flat_spots", heterogeneity_tsfeat_workaround, # "heterogeneity",
                 "holt_parameters", hw_parameters_tsfeat_workaround, # "hw_parameters", 
                 "hurst", "lumpiness", 
                 "nonlinearity", "pacf_features", 
                 "stl_features", "stability",  
                 "unitroot_kpss", "unitroot_pp"),
    scale = scaled
  ) %>% 
    tibble::add_column("n_observations" = length(.y)) %>%
    replace(is.na(.), 0)
  
  return(ts_features)
}


#' Workaround for heterogeneity features ----
#'
#' @param x                ts: a time series object
#'
#' @return a tibble with time series features
#'
#' @export
heterogeneity_tsfeat_workaround <- function(x) {
  output <- c(arch_acf =0, garch_acf=0, arch_r2=0, garch_r2=0)
  try( output <- tsfeatures::heterogeneity(x) )
  output
}

#' Workaround for Holt-Winters features ----
#'
#' @param x                ts: a time series object
#'
#' @return a tibble with time series features
#'
#' @export
hw_parameters_tsfeat_workaround <- function(x) {
  hw_fit <- NULL
  hw_fit$par <- c(NA, NA, NA)
  try(hw_fit <- forecast::ets(x, model=c("AAA")), silent=TRUE)
  names(hw_fit$par) <- c("hw_alpha", "hw_beta" , "hw_gamma")
  hw_fit$par[1:3]
}
