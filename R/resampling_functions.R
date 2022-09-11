
# possibility to include change-point detection to assess whether origin should be fixed or not
# in this case, the origin should be changed to the point after change-point while satisfying all other criteria

#' Determine split size 'initial' with fixed start point ----
#'
#' @param df_ts        tibble: data.frame to be resampled
#' @param horizon      numeric: how many steps ahead should be forecasted? also the 'assess' size
#' @param min_obs      numeric: the minimum amount of observations each split must have
#' @param max_splits   numeric: the maximum amount of splits to be considered (if null, all possible)
#' @param most_recent  logical: should only the most recent splits be considered? otherwise, randomly selected
#'
#' @return  list with all arguments to be passed in rsample::resample function
#'
#' @export
#'
resample_fixed_initial <- function(df_ts, horizon, min_obs, max_splits = NULL, most_recent = TRUE) {
  n_obs <- nrow(df_ts)

  # create resample object
  resamples <- rsample::rolling_origin(data = df_ts,
                                       initial = min_obs,
                                       assess = horizon,
                                       cumulative = TRUE
  )

  # total amount of resamples
  n_stops <- nrow(resamples)

  # if no max_splits or less stops than max_splits, then all resamples
  if (is.null(max_splits) | n_stops < max_splits) {
    resamples <- resamples
  }

  # if there are more stops than max_split & most_recent is true
  # then select the 'max_split' most recent splits
  if (n_stops > max_splits & most_recent) {
    resamples <- resamples %>%
      dplyr::slice(seq(to = n_stops, by = 1, length.out = max_splits))
  }
  # if there are more stops than max_split & most_fecent is false,
  # then select the 'max_split' splits randomly
  if (n_stops > max_splits & !most_recent) {
    resamples <- resamples %>%
      dplyr::sample_n(max_splits) %>%
      dplyr::arrange(id)
  }

  # restore dropped attributes from sampling (evaluate if others should be replaced as well)
  attr(resamples, 'class') <- c("rolling_origin", "rset", "tbl_df", "tbl", "data.frame")

return(resamples)
}
