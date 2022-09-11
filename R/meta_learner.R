#' Train meta-learner ----
#'
#' @param meta_matrix   tibble: the meta_matrix from the output of the wrapper2metalearning function
#' @param n_cores       numeric: number of cores to run the hyperparameter tuning
#' @param n_folds       numeric: number of splits in the cross-validation
#'
#' @return  a model
#'
#' @export
#'
train_meta <- function(meta_matrix, n_cores = 1, n_folds = 10) {
  
  # CROSS-VALIDATION
  df_folds <- meta_matrix %>%
    rsample::vfold_cv(v = n_folds, repeats = 2)
  
  # RECIPE 
  xgb_recipe <- recipes::recipe(best_method ~ ., data = meta_matrix) %>%
    recipes::step_normalize(all_numeric())
  
  # MODEL SPECIFICATION 
  xgb_model <- 
    parsnip::boost_tree(mtry = tune(), trees = tune(), min_n = tune(), tree_depth = tune(),
               learn_rate = tune(), loss_reduction = tune() ) %>% 
    parsnip::set_engine("xgboost", nthread = eval(n_cores)) %>% 
    parsnip::set_mode("classification") 
  
  # WORKFLOW
  xgb_workflow <- 
    workflows::workflow() %>% 
    workflows::add_model(xgb_model) %>% 
    workflows::add_recipe(xgb_recipe)
  
  # SET HYPERPARAMETERS RANGE
  xgb_parameters <- xgb_workflow %>%
    hardhat::extract_parameter_set_dials() %>%
    recipes::update(
      mtry = mtry(range = c(3L, 42L)),
      trees = trees(range = c(1000L, 2000L)),
      min_n = min_n(range = c(10L, 200L)),
      tree_depth = tree_depth(range = c(3L, 25L)),
      learn_rate = learn_rate(range = c(-5,-1)),
      loss_reduction = loss_reduction(range = c(0, 2))
    )
  
  # TUNE HYPERPARAMETERS (RACE ANOVA)
  Sys.time()
  xgb_final_tune <- 
    xgb_workflow %>%
    finetune::tune_race_anova(object = .,
                              resamples = df_folds,
                              metrics = metric_set(roc_auc),
                              control = finetune::control_race(burn_in = 3,
                                                               verbose = FALSE)
                              
                              )

  Sys.time()
  
  # SELECT BEST HYPERPARAMETERS
  xgb_best_model <-
    xgb_workflow %>%
    tune::finalize_workflow({tune::select_best(xgb_final_tune)})
  
  # CONF MATRIX 
  # xgb_confMatrix <- xgb_best_model %>%
  #   tune::last_fit(df_split) %>%
  #   workflowsets::collect_predictions() %>%
  #   yardstick::conf_mat(truth = y, estimate = .pred_class)
  
  # EXPORT MODEL
  result <- fit(xgb_best_model, meta_matrix)
  return(result)
}
