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
source('R/offline_phase.R')
source('R/online_phase.R')

# get data ----
m4_files <- list.files(recursive = TRUE)
m4_files <- m4_files[grepl(pattern = c('m4_data.rds'),
                           x = m4_files)]
m4_data <- readRDS(m4_files)

# sample ts for proof-of-concept ----
set.seed(15081991)
m4_data <- m4_data %>%
  mutate(min_obs = ifelse(frequency == 1, h, 2 * frequency)) %>%
  filter(n > ( min_obs + 2*h)) %>%
  # filter(frequency == 24) %>%
  sample_n(500)

# offline data phase ----
t0 <- Sys.time()
m4_data <- offline_data_phase(.data = m4_data, .n_cores = 3, .series_per_chunk = 5, .temp_file = 'teste_save.rds', .save_each = 3)
Sys.time() - t0

saveRDS(m4_data, file = 'db/m4_post_offline.rds', compress = FALSE)

# # validação erros
# data_testar <- data %>% filter(!(id %in% data$id)) # m4_data depois de validado
# data_testar <- m4_data %>% filter(!(id %in% m4_offline$id))
# teste <- vector('list', nrow(data_testar))
# Sys.time() # começo
# for (w in seq(33, nrow(data_testar))) {
#   teste[[w]] <- offline_data_phase(.data = data_testar[w, ])
#   print(w)
#   print(Sys.time())
# }
# 
# w <- c(12, # erro: from glmnet C++ code (error code 7777); All used predictors have zero variance
#        13, # erro: y is constant; gaussian glmnet fails at standardization step
#        14, # erro: y is constant; gaussian glmnet fails at standardization step
#        32  # erro: need at least two non-NA values to interpolate
#        )

# offline meta_learner phase ----
t0 <- Sys.time()
m4_metalearner <- offline_metalearner_phase(.data = m4_data,
                                            .balance = TRUE,
                                            .n_cores = 3)
Sys.time() - t0

saveRDS(m4_metalearner, file = 'db/m4_metalearner.rds', compress = FALSE)


# online phase ----
t0 <- Sys.time()
m4_temp <- online_phase(.data = m4_data,
                        .learner = m4_metalearner,
                        .n_cores = 3,
                        .series_per_chunk = 5)
Sys.time() - t0

