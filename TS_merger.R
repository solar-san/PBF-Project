all_ts_list <- split(
  x = tmelt, 
  f = tmelt$variable
  )

all_ts_list %>% class
all_ts_list %>% names

no_missing_values_all_ts <- all_ts_list %>% 
  lapply(
        X = ., 
    FUN = drop_na
    )

no_missing_values_all_ts %>% 
  lapply(X = ., FUN = dim) %>% 
  unique
