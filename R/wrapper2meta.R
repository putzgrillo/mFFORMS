#' calculate the optimal weights for each  ----
#'
#' @param resamples    tibble: tibble generated from from rsample package
#' @param ...          arguments to be passed in the combination methods
#'
#' @return  a matrix with weights
#'
#' @export
#'
weights_oos <- function(resamples, ...) {
  #
  resamples %>%
    mutate(
      # run forecast methods
      forecasts = purrr::map(
        splits,
        ~forecast_methods(.y = rsample::analysis(.x) %>% as_ts(),
                          .h = nrow(rsample::assessment(.x)))$mean    # return only mean
      ),
      # calculate weights
      weights = purrr::pmap(
        .l = list(splt = splits, fcts = forecasts),
        .f = function(splt, fcts) {
          wrapper2weights(.actuals = rsample::assessment(splt) %>% as_ts(),
                          .forecasts = fcts, ...)
        })
    ) %>% pull(weights) 
}


#' wrapper on splits to feed meta-learning ----
#'
#' @param resamples    tibble: tibble generated from from rsample package
#' @param weights      matrix: a weight matrix from the output of weights_oos function
#'
#' @return  tibble with all data to build a matrix for meta-learning model
#'
#' @export
#'
wrapper2metalearning <- function(resamples, weights, ...) {
  # calculate steps
  resamples %>%
    mutate(
      weights = weights
    ) %>%
    mutate(
      # calculate ts features 
      ts_features = purrr::map(
        splits,
        ~ts_features(.y = rsample::analysis(.x) %>% as_ts(),
                     scaled = TRUE)
      ),
      # run forecast methods
      forecasts = purrr::map(
        splits,
        ~forecast_methods(.y = rsample::analysis(.x) %>% as_ts(),
                          .h = nrow(rsample::assessment(.x)))$mean    # return only mean
      ),
      # generate combinations 
      combination_forecasts = purrr::pmap(
        .l = list(wgts = weights, fcts = forecasts),
        .f = function(wgts, fcts) {
          weights2combination(.weights = wgts,
                              .forecasts = fcts)
        }),
      # calculate accuracy measures for combinations
      combination_errors = purrr::pmap(
        .l = list(splt = splits, cmbn_fcts = combination_forecasts),
        .f = function(splt, cmbn_fcts) {
          accuracy_measures(.actuals = rsample::assessment(splt) %>% as_ts(), 
                            .forecasts = cmbn_fcts, 
                            .historical = rsample::analysis(splt) %>% as_ts())
          
        }),
      # create training matrix
      meta_matrix = purrr::pmap(
        .l = list(fts = ts_features, acc = combination_errors),
        .f = function(fts, acc) {
          create_classification_table(.features = fts, .accuracy = acc)
        }
      )
    ) %>%
    select(combination_forecasts, combination_errors, meta_matrix)
}
