#' Compute accuracy measures 
#' 
#' @param .actuals        ts: a ts with actual values that were predicted
#' @param .forecasts      mts: a mts with predicted values
#' @param .historical     ts: a ts with historical values used to generate the predictions (to calculate MASE)
#' @param .measures       character: name of the accuracy measures to be computed
#'
#' @return  a tibble with accuracy measures
#'
#' @export
#'
accuracy_measures <- function(.actuals, .forecasts, .historical, 
                              .measures = c('ME', 'MAE', 'RMSE', 'MAPE', 'MASE')) {
  # names of methods
  nms_models <- colnames(.forecasts)
  
  # get errors
  errors <- .actuals - .forecasts
  colnames(errors) <- nms_models
  
  # calculate measures 
  .accuracy <- vector('list', length(.measures))
  for (w in seq_along(.accuracy)) {
    .accuracy[[w]] <- do.call(what = .measures[w], 
                              args = list(.residual = errors,
                                          .actual = .actuals,
                                          .train = .historical))
  }
  
  # return tidy object
  dplyr::bind_rows(.accuracy) %>%
    tidyr::pivot_longer(-.model, names_to = 'metric', values_to = 'value') %>%
    na.omit() %>%
    tidyr::pivot_wider(names_from = 'metric', values_from = 'value')

}

#' Compute ME 
#' 
#' @param .residuals     ts: a mts with residuals
#'
#' @return  a tibble with ME
#'
#' @export
#'
ME <- function(.residuals, ...) {
  lapply(.residuals, function(residuals) {mean(residuals)}) %>%
    as_tibble() %>% 
    pivot_longer(everything(), names_to = '.model', values_to = 'ME')
}


#' Compute MAE 
#' 
#' @param .residuals     ts: a mts with residuals
#'
#' @return  a tibble with MAE
#'
#' @export
#'
MAE <- function(.residuals, ...) {
  lapply(.residuals, function(residuals) {mean(abs(residuals))}) %>%
    as_tibble() %>% 
    pivot_longer(everything(), names_to = '.model', values_to = 'MAE')
}

#' Compute RMSE
#' 
#' @param .residuals     ts: a mts with residuals
#'
#' @return  a tibble with RMSE
#'
#' @export
#'
RMSE <- function(.residuals, ...) {
  lapply(.residuals, function(residuals) {sqrt(mean(residuals ** 2))}) %>%
    as_tibble() %>% 
    pivot_longer(everything(), names_to = '.model', values_to = 'RMSE')
}

#' Compute MAPE
#' 
#' @param .residuals     ts: a mts with residuals
#' @param .actual        ts: a ts with actual values
#'
#' @return  a tibble with MAPE
#'
#' @export
#'
MAPE <- function(.residuals, .actual, ...) {
  # calculate percentage errors
  PE <- .residuals / .actual * 100
  colnames(PE) <- colnames(.residuals)
  # calculate MAPE
  lapply(PE, function(residuals) {mean(abs(residuals))}) %>%
    as_tibble() %>% 
    pivot_longer(everything(), names_to = '.model', values_to = 'MAPE')
}

#' Compute in-sample seasonal Naïve MASE
#' 
#' @param .residuals     ts: a mts with residuals
#' @param .train         ts: a ts with historical data used in the model training
#'
#' @return  a tibble with MASE
#'
#' @export
#'
MASE <- function(.residuals, .train, ...) {
  # calculate scale
  .freq <- frequency(.residuals)
  .scale <- mean(abs(base::diff(.train, lag = .freq)))
  
  # calculate MASE
  lapply(.residuals, function(residuals) {mean(abs(residuals) / .scale)}) %>%
    as_tibble() %>% 
    pivot_longer(everything(), names_to = '.model', values_to = 'MASE')
}

