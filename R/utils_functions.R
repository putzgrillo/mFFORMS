
create_classification_table <- function(.features, .accuracy) {
  # filter best combination
  bst_comb <- .accuracy %>%
    dplyr::arrange(MASE) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.model)
  
  # create table
  tibble(
    best_method = bst_comb,
    .features
  )
}


as_mts <- function(y_mts, x_date, freq) {
  y_mts <- as.data.frame(y_mts)
  
  mts <- lapply(y_mts, function(x) {
    ts(x, frequency = freq, start = min(x_date), end = max(x_date))
  }) |> do.call(what = cbind)
  
  return(mts)
}
