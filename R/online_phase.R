#' mFFORMS online phase ----
#'
#' @param .data             tibble: the m4_data tibble (or in the same format)
#' @param .learner          xgboost: the tidymodels xgboost output from the offline_phase
#' @param .n_cores          numeric: number of cores to process parallel
#' @param .series_per_chunk numeric: number of time series to run per core * iteration
#'
#' @return .data with inclusion of the predicted label and expected forecast
#'
#' @export
#'
online_phase <- function(.data, .learner, .n_cores, .series_per_chunk = 10) {
  # calculate the features to pass to meta_learner
  .data <- .data %>%
    dplyr::mutate(
      features = purrr::map(ts_historical, ~ts_features(.x))
    ) 
  
  # calculate the features for each series
  ## obs: calculating and binding to avoid adjusting manually the ts_features output
  ## so, if there is at least one seasonal in the new_series, it will adjust to 0
  new_meta <- .data %>% 
    dplyr::pull(features) %>%
    dplyr::bind_rows() %>%
    replace(is.na(.), 0)
  
  # predict the label for each series
  .data <- .data %>%
    mutate(
      learner_label = predict(.learner, new_data = new_meta) %>% unlist()
    ) 
  
  # create resamples to calculate weights for methods that require
  .data <- .data %>%
    mutate(
      # create splits to select the combination method
      resamples = purrr::pmap(
        .l = list(tsrs = ts_historical, hrzn = h, min_n = min_obs),
        .f = function(tsrs, hrzn, min_n) {
          resample_fixed_initial(df_ts = tsrs,  
                                 horizon = hrzn,
                                 min_obs = min_n, 
                                 most_recent = T,   # only the most recent, as FFORMA
                                 max_splits = 1)}   # consider only one, as FFORMA
      )
    )
    
  # calculate predictions with smart-approach
  .data <- .data %>%
    mutate(
      # generate combined predictions 
      predictions = purrr::pmap(
        .l = list(resample = resamples, label = learner_label),
        .f = function(resample, label) {
          smart_combination(.resample = resample$splits[[1]],  # ugly hack to avoid adjusting prediction to multiple resamples
                            .label = as.character(label))      # output as character, otherwise do.call doesnt work properly
        }
      )
    )
  
  
  ## parallelization: parallelization parameters ----
  ### create temporary files (to reduce memory usage)
  max_series_iteration <- .n_cores * .series_per_chunk # number of series to calculate
  .n_series <- nrow(.data)
  index_lower <- seq(from = 1, to = .n_series, by = max_series_iteration)
  index_upper <- c(index_lower[-1] - 1, .n_series)
  
  ## parallelization: start procedure ----
  ts_simulation <- vector("list")
  library(parallel)
  # START_PROCEDURE
  for (w in seq_along(index_lower)) {
    # create temp df with max_series_iteration series
    temp_data <- .data %>% dplyr::slice(seq(index_lower[w], index_upper[w]))
    
    # break it into n_core lists
    temp_data$id_core <- paste('c_', sample(seq(.n_cores), size = nrow(temp_data), replace = T), sep = '')
    temp_data <- split(temp_data, f = temp_data$id_core)
    
    # run procedure
    temp_simulation <- mclapply(temp_data, function(x) {
      # # # # # # # # # # # # # # # # # # # # # # # # 
      # # # # # # código tidy, restante # # # # # # #
      # # # # # # é para paralelização # # # # # # #
      # # # # # # # # # # # # # # # # # # # # # # # # 
      x %>%
        mutate(
          # generate combined predictions 
          predictions = purrr::pmap(
            .l = list(resample = resamples, label = learner_label),
            .f = function(resample, label) {
              smart_combination(.resample = resample$splits[[1]],  # ugly hack to avoid adjusting prediction to multiple resamples
                                .label = as.character(label))      # output as character, otherwise do.call doesnt work properly
            }
          )
        )
    }, mc.cores = .n_cores)
    
    # append 
    ts_simulation <- append(ts_simulation, temp_simulation)
  }
  
  # bind rows (removing computed chunks with errors)
  names(ts_simulation) <- NULL                                  # cannot have duplicated names
  index2keep <- lapply(ts_simulation, is_tibble) %>% unlist()   # get only positions without errors
  .data <- dplyr::bind_rows(ts_simulation[index2keep])
  
  # return data without intermediate columns
  .data %>%
    select(-resamples)
}


# for (w in seq(nrow(.data))) {
#   smart_combination(
#     .resample <- .data$resamples[[w]]$splits[[1]],
#     .label <- as.character(.data$learner_label[[w]] )
#   )
#   print(w)
#   print(Sys.time())
# }

#' forecasting optimised for the the selected combination method ----
#'
#' @param .resample         split: a rsample split (to be adjusted to deal with multiple splits)
#' @param .label            character: the combination method to be run
#'
#' @return a combined forecast time series
#'
#' @export
smart_combination <- function(.resample, .label) {
  # calculate full time series
  .full_tsibble <- bind_rows(rsample::analysis(.resample), 
                             rsample::assessment(.resample)) 
  
  # number of steps ahead, since the resample comes from within the online_phase, it will have the right amount of steps to predict
  .steps_ahead <- nrow(rsample::assessment(.resample)) 
  
  # 1N combination ----
  # if is 1/N, predict all and assign weights without calculating out-of-sample
  if (grepl('1N', .label)) {
    # full time series
    forecasts <- forecast_methods(.y = as_ts(.full_tsibble),
                                  .h = .steps_ahead)$mean
    # calculate weights
    weights <- wrapper2weights(.actuals = as_ts(.full_tsibble),
                               .forecasts = forecasts,
                               .weight_methods = .label)
    # combine predictions
    combined_forecast <- weights2combination(.forecasts = forecasts,
                                             .weights = weights)
    
    # stop function and return the combined forecasts
    return(combined_forecast)
  }
  
  # other combinations ----
  ### if others, calculate all forecasts, assign weights with out-of-sample error . 
  ### then run only methods selected with full data_set
  
  # calculate forecasts with training data
  test_period_forecasts <- forecast_methods(.y = as_ts(rsample::analysis(.resample)),
                                            .h = .steps_ahead)$mean
  
  # calculate weights
  weights_oos <- wrapper2weights(.actuals = as_ts(rsample::assessment(.resample)),
                                 .forecasts = test_period_forecasts,
                                 .weight_methods = .label)
  
  ## error adjustment if all weights are zero (for lasso and peLASSO)
  if (sum(weights_oos) == 0) {
    weights_oos[1,] <- rep(x = 1 / ncol(weights_oos), times = ncol(weights_oos))
  }
  
  # calculate forecasts for selected methods
  methods_to_forecast <- colnames(weights_oos)[weights_oos > 0]
  forecasts <- forecast_methods(.y = as_ts(.full_tsibble),
                                .h = .steps_ahead,
                                .forecast_methods = methods_to_forecast)$mean
  
  # filter weights of the selected methods
  weights_oos <- weights_oos[, colnames(weights_oos) %in% methods_to_forecast, drop = FALSE]
  
  # combine forecasts with oos weights
  weights2combination(.forecasts = forecasts,
                      .weights = weights_oos)
}
