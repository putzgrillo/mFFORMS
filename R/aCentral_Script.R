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
  filter(n > ( min_obs + 2*h)) %>%
  filter(frequency == 24) %>%
  sample_n(20)

# calcular dados para meta 
t0 <- Sys.time()
data <- offline_data_phase(.data = m4_data, .n_cores = 2)
Sys.time() - t0

saveRDS(m4_data, file = 'db/m4_post_offline.rds', compress = FALSE)


# validação erros
data_testar <- m4_data %>% filter(!(id %in% data$id))
teste <- vector('list', nrow(data_testar))
Sys.time() # começo
for (w in seq_along(teste)) {
  teste[[w]] <- offline_data_phase(.data = data_testar[w, ])
  print(w)
  print(Sys.time())
}


# calcular meta_learner








# generate meta-learner ----
    ## create training data
meta_data <- m4_data %>%
  pull(meta) %>% bind_rows() %>%
  pull(meta_matrix) %>% bind_rows() %>%
  replace(is.na(.), 0)

    ## balance classes
meta_balanced <- meta_data %>%
  group_by(best_method) %>%
  sample_n(364, replace = T)

    ## fit classifier ###
t0 <- Sys.time()
# m4_metalearner <- train_meta(meta_data, n_folds = 3, n_cores = 3)
m4_metalearner <- train_meta(meta_balanced, n_folds = 5, n_cores = 3)
Sys.time() - t0

saveRDS(m4_metalearner, file = 'db/m4_metalearner.rds', compress = FALSE)

# evaluate predictions (classes that are being assigned)
.data <- m4_data %>% sample_n(100)
.learner <- m4_metalearner
.new_meta <- .data %>%
  pull(meta) %>% bind_rows() %>%
  pull(meta_matrix) %>% bind_rows() %>%
  replace(is.na(.), 0)

.data %>%
  # mutate(learner_label = predict(.learner, new_data = .new_meta)) %>% 
  count(learner_label) %>% mutate(freq = n / sum(n))

meta_data %>% count(best_method) %>% mutate(freq = n / sum(n))

# ## not a very honest review (different series), but an approximation
# .data %>%
#   mutate(
#     learner_label = unlist(predict(.learner, new_data = .new_meta)),
#     actual = meta_data$best_method
#   ) %>% 
#   count(learner_label, actual) %>%
#   pivot_wider(names_from = learner_label, values_from = n) %>%
#   arrange(actual)


# generate predictions  ----
.data <- .data %>% 
  mutate(learner_label = predict(.learner, new_data = .new_meta)) 

.data %>%
  mutate(previsoes = purrr::map())

