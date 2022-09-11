# install package with m4 data ----
options(timeout = 10000)
install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz",
                 repos=NULL)
# get data ----
library(M4comp2018)
data("M4")

# function ts into tibble ----
ts2tibble <- function(ts_object, period = c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")) {
  
  # ts objects all ill-suited for hourly data, for this reason it has to be adjusted
  if (period %in% c("Hourly")) {
    # by default, make_datetime() is datetime origin (1970-01-01 00:00:00)
    ts_index <- {lubridate::make_datetime() + lubridate::dhours(seq(length(ts_object)))} %>%
      lubridate::ceiling_date(unit = 'hours')
    
    # if as_tsibble directly, it will explode memory when reversing to ts (as.ts())
    df_ts <- tsibble(Time = ts_index, 
                     Date = lubridate::date(ts_index), 
                     value = as.numeric(ts_object), 
                     index = 'Time')
  } else{
    df_ts <- as_tsibble(ts_object)
  }
  
  # return object
return(df_ts)
}

# adjust list format ----
  # # remove external forecasts
  # # create data.frame with 'date' and 'y' variables (easier to split within tidymodels framework)
library(tidyverse)
library(lubridate)
library(tsibble)
# set.seed(193827)

M4 <- tibble(M4)

t0 <- Sys.time()
m4_data <- M4 %>%
  mutate(
    id = map(M4, ~.x$st) %>% unlist(),
    type = map(M4, ~.x$type) %>% unlist(),
    period = map(M4, ~.x$period) %>% unlist(),
    start = map(M4, ~start(.x$x)),
    frequency = map(M4, ~frequency(.x$x)) %>% unlist(),
    n = map(M4, ~.x$n) %>% unlist(),
    h = map(M4, ~.x$h) %>% unlist(),
    ts_historical = map(M4, ~ts2tibble(ts_object = .x$x,
                                       period = .x$period)),
    ts_future = map(M4, ~ts2tibble(ts_object = .x$xx,
                                       period = .x$period))
  ) %>% select(-M4)

Sys.time() - t0

# export adjusted list & sample of adjusted list ----
  # # full data
saveRDS(m4_data, file = 'db/m4_data.rds')

# M4[[1]]$x      # yearly
# M4[[23001]]$x  # quarterly
# M4[[47991]]$x  # monthly
# M4[[95347]]$x  # weekly
# M4[[96350]]$x  # daily
# M4[[100000]]$x # hourly

