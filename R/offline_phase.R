# .data <- readRDS(m4_files)
# set.seed(15081991)
# .data <- .data %>%
#   # sample_n(300) %>%
#   mutate(min_obs = ifelse(frequency == 1, h, 2 * frequency)) %>%
#   filter(n >( min_obs + 2*h)) %>%
#   sample_n(20)

#' mFFORMS offline phase: data gather ----
#'
#' @param .data             tibble: the m4_data tibble (or in the same format)
#' @param .n_cores          numeric: number of cores to process parallel
#' @param .series_per_chunk numeric: number of time series to run per core * iteration
#' @param .temp_file        character: full path to save Rds file
#' @param .save_each        numeric: after how many iterations should it be saved?
#'
#' @return the .data with included meta column
#'
#' @export
#'
offline_data_phase <- function(.data, .n_cores = 1, .series_per_chunk = 10, .temp_file = NULL, .save_each = 100) {
  # create data splits ----
  .data <- .data %>%
    mutate(
      # create splits to estimate combination weights
      resamples4weights = purrr::pmap(
        .l = list(tsrs = ts_historical, hrzn = h, min_n = min_obs, n_obs = n),
        .f = function(tsrs, hrzn, min_n, n_obs) {
          resample_fixed_initial(df_ts = tsrs %>% filter(row_number() <= n_obs - hrzn),  # select non-overlapping
                                 horizon = hrzn,
                                 min_obs = min_n,
                                 most_recent = T,
                                 max_splits = 1)}
      ),
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
  
  # apply function to return the data to construct the meta-learner ----
  
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
      x %>%
        mutate(
          # calculate weights based on non-overlaping period (training-test)
          weights_oos = purrr::map(resamples4weights, ~weights_oos(.x))
        ) %>% 
        select(-resamples4weights) %>%
        mutate(
          # calculate the test period errors of the combination methods
          meta = purrr::pmap(
            .l = list(rsmpl = resamples, wgts = weights_oos),
            .f = function(rsmpl, wgts) {
              wrapper2metalearning(resamples = rsmpl, weights = wgts)
            })
        )
    }, mc.cores = eval(.n_cores))
    
    # append 
    ts_simulation <- append(ts_simulation, temp_simulation)
    
    # # # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # control parameters # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # # # 
    # print progress
    print(
      paste0(w, '/', length(index_lower), ' at ', Sys.time())
    )
    
    # save_temp file
    if (!is.null(.temp_file) & w %% .save_each == 0) {
      saveRDS(ts_simulation, file = .temp_file, compress = FALSE)
    }
    
    # # # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # end control parameters
    # # # # # # # # # # # # # # # # # # # # # # # 
    
  }
  
  # bind rows (removing computed chunks with errors)
  names(ts_simulation) <- NULL                                  # cannot have duplicated names
  index2keep <- lapply(ts_simulation, is_tibble) %>% unlist()   # get only positions without errors
  .data <- dplyr::bind_rows(ts_simulation[index2keep])

  # remove temporary columns
  .data <- .data %>%
    dplyr::select(-resamples, -id_core, -weights_oos)
  
  # return the data updated with meta infos
return(.data)
}


#' mFFORMS offline phase: train meta-learner model  ----
#'
#' @param .data             tibble: the m4_data tibble output from offline_data_phase (or in the same format)
#' @param .baance           logical: should the classes be balanced before classification
#' @param .nfolds           numeric: number of folds to run cross-validation on
#' @param .n_cores          numeric: number of cores to process parallel
#'
#' @return a tidymodels object
#'
#' @export
#'
offline_metalearner_phase <- function(.data, .balance = FALSE, .n_folds = 5, .n_cores = 1) {
  # create the matrix for meta-learning ----
  meta_data <- .data %>%
    pull(meta) %>% bind_rows() %>%
    pull(meta_matrix) %>% bind_rows() %>%
    replace(is.na(.), 0)
  
  # balance classes
  if (.balance) {
    # max observations in the least frequent class
    max_obs <- meta_data %>%
      count(best_method) %>%
      arrange(n) %>%
      slice(1) %>% pull(n)
    
    # sample max_obs for each prediction class
    meta_data <- meta_data %>%
      group_by(best_method) %>%
      sample_n(max_obs, replace = T) %>% 
      ungroup()
  }
  
  # fit the meta-learner ----
  meta_learner <- train_meta(meta_data, 
                             n_folds = .n_folds, 
                             n_cores = .n_cores)
  
  # return the data updated with meta infos
  return(meta_learner)
}