# possibility to include change-point detection to assess whether origin should be fixed or not
# in this case, the origin should be changed to the point after change-point while satisfying all other criteria

#' Apply selected methods on resample time  ----
#'
#' @param .y                ts: a time series to be predicted
#' @param .h                numeric: number of steps ahead to be predicted
#' @param .alpha            numeric: significance level
#' @param .forecast_methods character: vector of forecast models to generate
#'
#' @return a list with predicted value, upper bound and lower bound
#'
#' @export
forecast_methods <- function(
    .y, .h, .alpha = 0.05,
    .forecast_methods = c("forecast_arima", "forecast_ets", "forecast_naive",
                          "forecast_rwd", "forecast_snaive", "forecast_stlm", 
                          "forecast_tbats", "forecast_thetaf"#, 'forecast_nnetar'
                          )
    ) {
  # adjust if is passed as tsibble
  if (tsibble::is_tsibble(.y)) {
    .y <- .y %>% as.ts()
  }
  
  # RUN ALL METHODS
  forecasts <- lapply(setNames(as.list(.forecast_methods), .forecast_methods), 
                      function(x) {do.call(what = x,
                                           args = list(y = .y, h = .h, cl = (1-.alpha) * 100 ))
                      })
  
  list_forecasts <- list(
    mean = lapply(forecasts, function(x) {x$mean}) |> do.call(what = cbind),
    # if residuals are not considered to be symmetric,
    lower = lapply(forecasts, function(x) {x$lower}) |> do.call(what = cbind),
    upper = lapply(forecasts, function(x) {x$upper}) |> do.call(what = cbind) 
    # if residuals are to be considered symmetric (radius saves ~226MB for 9 models, h = 5 and 100,000 series)
    #                                                     saves ~488MB for 9 models, h = 24 and 100,000 series)
    # radius = lapply(forecasts, function(x) {x$upper - x$lower}) |> do.call(what = cbind)
  )
  return(list_forecasts)
}

#' Forecast ARIMA  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_arima <- function(y, h, cl) {
  forecast::forecast(object = forecast::auto.arima(y = y, 
                                                   stepwise = TRUE, 
                                                   approximation = FALSE),
                     h = h,
                     level = cl)
}

#' Forecast ETS  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_ets <- function(y, h, cl) {
  forecast::forecast(object = forecast::ets(y = y,opt.crit = "mse"),
                     h = h,
                     level = cl) 
}

#' Forecast TBATS  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_tbats <- function(y, h, cl) {
  forecast::forecast(object = forecast::tbats(y = y, use.parallel = FALSE),
                     h = h,
                     level = cl) 
}

#' Forecast THETAF  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_thetaf <- function(y, h, cl) {
  forecast::thetaf(y = y, h = h, level = cl)
}

#' Forecast RW w/ DRIFT  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_rwd <- function(y, h, cl) {
  forecast::rwf(y = y, drift = TRUE, h = h, level = cl) 
}

#' Forecast STLM-AR  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_stlm <- function(y, h, cl) {
  model <- tryCatch({
    forecast::stlm(y, modelfunction = stats::ar)
  }, error = function(e) forecast::auto.arima(y, d = 0, D = 0))
  forecast::forecast(model, h = h, level = cl)
}

#' Forecast NAIVE  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_naive <- function(y, h, cl) {
  forecast::naive(y = y, h = h, level = cl)
}

#' Forecast SNAIVE  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_snaive <- function(y, h, cl) {
  if(frequency(y) == 1) {
    forecast::naive(y = y, h = h, level = cl)
  } else {
    forecast::snaive(y = y, h = h, level = cl)
  }
}

#' Forecast NNETAR  ----
#'
#' @param y                ts: a time series to be predicted
#' @param h                numeric: number of steps ahead to be predicted
#' @param cl               numeric: confidence level
#'
#' @return a ts of predicted values
#'
#' @export
forecast_nnetar <- function(y, h, cl) {
  forecast::forecast(object = forecast::nnetar(y = y),
                     h = h,
                     level = cl) 
}