#' Combine forecasts ----
#'
#' @param .forecasts      mts: a mts with predicted values
#' @param .weights        matrix: the weight matrix output from wrapper2weights
#'
#' @return  a mts with the combination methods
#'
#' @export
#'
weights2combination <- function(.forecasts, .weights) {

  weighted_fcts <- .forecasts %*% t(.weights)
  weighted_fcts <- as_mts(y_mts = weighted_fcts,
                          x_date = time(.forecasts),
                          freq = frequency(.forecasts))
  
  return(weighted_fcts)
}


#' wrapper to calculate weights ----
#'
#' @param .actuals        ts: a ts with actual values
#' @param .forecasts      mts: a mts with predicted values
#' @param .weight_methods character: vector of combination methods to employ
#'
#' @return  a matrix with weights for each model 
#'
#' @export
#'
wrapper2weights <- function(.actuals, 
                            .forecasts, 
                            .weight_methods = c("weights_peLASSO",
                                                "weights_LASSO",
                                                "weights_1N",
                                                "weights_minrmse")) {
  # GET WEIGHTS
  weights_matrix <- lapply(setNames(as.list(.weight_methods), .weight_methods), 
                           function(x) {do.call(what = x,
                                                args = list(.actual = .actuals, 
                                                            .forecast = .forecasts))
                           }) |> do.call(what = rbind)
  
  return(weights_matrix)
}

#' equal weights combinations from models ----
#'
#' @param .actual      ts: a ts with actual values
#' @param .forecast    ts: a ts with predicted values
#'
#' @return  a weights matrix
#'
#' @export
#'
weights_1N <- function(.actual, .forecast) {
  n_methods <- ncol(.forecast)
  weights_1n <- 1 / rep(n_methods, n_methods)
  names(weights_1n) <- colnames(.forecast)
return(weights_1n)
}


#' calculate peLasso weights ----
#'
#' @param .actual        ts: a ts with actual values
#' @param .forecast      ts: a ts with predicted values
#' @param .constraint    logical: should weights sum to one? (peLasso article defaults to FALSE)
#'
#' @return  named vector with weights
#'
#' @export
#'
weights_peLASSO <- function(.actual, .forecast, .constraint = FALSE) {
  # adjust ts to matrix
  .actual <- .actual %>% as.matrix()
  .forecast <- .forecast %>% as.matrix()
  
  # first step, lasso
  cv_lasso <- glmnet::cv.glmnet(y = .actual,
                                x = .forecast,
                                alpha = 1,
                                nfolds = 5,
                                intercept = FALSE)

  weights_lasso <- coef(cv_lasso, s = "lambda.1se")[-1,]

  # second step, eRidge (only if more than one variable left)
  n_nonzero <- sum(weights_lasso > 0)
  if (n_nonzero > 1) {
    # calculate adjusted actual (adapt eRidge)
    adjusted_actual <- .actual - apply(.forecast, 1, mean)
    # standard ridge
    cv_pelasso <- glmnet::cv.glmnet(y = adjusted_actual,
                                    x = .forecast[, weights_lasso != 0],
                                    family = "gaussian",
                                    alpha = 0,
                                    nfolds = 5,
                                    intercept = FALSE)

    weights_pelasso <- coef(cv_pelasso, s = "lambda.1se")[-1,]
  } else {
    weights_pelasso <- weights_lasso[weights_lasso != 0]
  }

  # if constraint, set sum of weigths to one
  if (.constraint) { weights_pelasso <- weights_pelasso / sum(weights_pelasso)  }

  # return results
  weights <- weights_lasso                               # all first step weights
  weights[weights_lasso != 0] <- weights_pelasso         # substitute weights
  return(weights)
}


#' minimum rmse weights combinations from out-of-sample errors ----
#'
#' @param .actual        ts: a ts with actual values
#' @param .forecast      ts: a ts with predicted values
#'
#' @return named vector with weights
#'
#' @export
#'
weights_minrmse <- function(.actual, .forecast) {
  
  # amount of methods
  nms_models <- colnames(.forecast)

  # get errors
  errors <- .actual - .forecast 
  colnames(errors) <- nms_models
  
  # select minimum rmse
  ind_best <- apply(errors, 2, function(x) {sum(x**2)}) %>% which.min()
  
  # assign weights
  weights <- vector('numeric', length(nms_models))
  names(weights) <- nms_models
  weights[ind_best] <- 1
  
  return(weights)
}


#' calculate LASSO weights ----
#'
#' @param .actual        ts: a ts with actual values
#' @param .forecast      ts: a ts with predicted values
#' @param .constraint    logical: should weights sum to one? (peLasso article defaults to FALSE)
#'
#' @return  named vector with weights
#'
#' @export
#'
weights_LASSO <- function(.actual, .forecast, .constraint = FALSE) {
  # adjust ts to matrix
  .actual <- .actual %>% as.matrix()
  .forecast <- .forecast %>% as.matrix()
  
  # first step, lasso
  cv_lasso <- glmnet::cv.glmnet(y = .actual,
                                x = .forecast,
                                alpha = 1,
                                nfolds = 5,
                                intercept = FALSE)

  weights_lasso <- coef(cv_lasso, s = "lambda.1se")[-1,]

  # if constraint, set sum of weigths to one
  if (.constraint) { weights_lasso <- weights_lasso / sum(weights_lasso)  }

  # return results
  weights <- weights_lasso
  return(weights)
}

