library(tsibble)
library(forecast)
library(tsfeatures)
library(tidymodels)
library(finetune)
library(tidyverse)
library(lubridate)


# strings
source('R/utils_functions.R')
source('R/resampling_functions.R')
source('R/ts_features.R')
source('R/forecast_methods.R')
source('R/combination_methods.R')
source('R/accuracy_measures.R')
source('R/wrapper2meta.R')
source('R/meta_learner.R')

# get data ----
m4_files <- list.files(recursive = TRUE)
m4_files <- m4_files[grepl(pattern = c('m4_data.rds'),
                           x = m4_files)]
m4_data <- readRDS(m4_files)

# sample para teste de conceito do código
set.seed(15081991)
m4_data <- m4_data %>%
  # sample_n(300) %>%
  mutate(min_obs = ifelse(frequency == 1, h, 2 * frequency)) %>%
  filter(n >( min_obs + 2*h)) %>%
  sample_n(1500)

## ts out-of-sample: ts resample ----
t0 <- Sys.time()
m4_data <- m4_data %>%
  mutate(
    # create resamples to estimate optimal out-of-sample weights
    resamples4weights = purrr::pmap(
      .l = list(tsrs = ts_historical, hrzn = h, min_n = min_obs, n_obs = n),
      .f = function(tsrs, hrzn, min_n, n_obs) {
        resample_fixed_initial(df_ts = tsrs %>% filter(row_number() <= n_obs - hrzn),  # select non-overlapping
                               horizon = hrzn,
                               min_obs = min_n,
                               most_recent = T,
                               max_splits = 1)}
      ),
    # create resamples to create meta learning (last h historical as out-of-sample) 
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
Sys.time() - t0

## proof of concept: parallelization ----

# create temporary files (to reduce memory usage)
max_series_iteration <- 25
.n_series <- nrow(m4_data)
index_lower <- seq(from = 1, to = .n_series, by = max_series_iteration)
index_upper <- c(index_lower[-1] - 1, .n_series)

ts_simulation <- vector("list")
# call parallel packages
library(parallel)
nucleos <- parallel::detectCores() - 1

# START_PROCEDURE
for (w in seq_along(index_lower)) {
  # create temp df with max_series_iteration series
  temp_m4_data <- m4_data %>% dplyr::slice(seq(index_lower[w], index_upper[w]))
  
  # break it into n_core lists
  temp_m4_data$id_core <- paste('c_', sample(seq(nucleos), size = nrow(temp_m4_data), replace = T), sep = '')
  temp_m4_data <- split(temp_m4_data, f = temp_m4_data$id_core)

  # run procedure
  temp_simulation <- mclapply(temp_m4_data, function(x) {
    # # # # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # código tidy, restante # # # # # # #
    # # # # # # é para paralelização # # # # # # #
    x %>%
      mutate(
        # calculate weights based on non-overlaping period
        weights_oos = purrr::map(resamples4weights, ~weights_oos(.x))
      ) %>% 
      select(-resamples4weights) %>%
      mutate(
        # calculate 
        meta = purrr::pmap(
          .l = list(rsmpl = resamples, wgts = weights_oos),
          .f = function(rsmpl, wgts) {
            wrapper2metalearning(resamples = rsmpl, weights = wgts)
          })
      )
  }, mc.cores = nucleos)
  
  # append 
  ts_simulation <- append(ts_simulation, temp_simulation)
  #
  print(Sys.time())
}

m4_data <- bind_rows(ts_simulation)
rm(ts_simulation)

meta_data <- m4_data %>%
  pull(meta) %>% bind_rows() %>%
  pull(meta_matrix) %>% bind_rows()

# calculate meta-learner ----
m4_metalearner <- train_meta(meta_data, n_folds = 2)

# 
la <- m4_data$ts_historical[[2]] %>%
  as.ts() %>%
  ts_features()

predict(m4_metalearner, new_data = la)

m4_data$ts_historical[[2]] %>%
  as.ts() %>%
  forecast_methods(.y = ., .h)
