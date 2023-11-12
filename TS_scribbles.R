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
  lapply(X = ., FUN = nrow ) 

series_length <- c()
for(ts in seq_along(tourism_full)){

  series_length[ts] <- nrow(tourism_full) - tourism_full[, ts] %>% is.na %>% sum
}



tmelt %>% 
  group_by(variable) %>% 
  mutate(series_length = 43-  value %>% is.na %>% sum) %>% 
  ungroup() %>% 
  arrange(desc(series_length)) %>% 
  mutate(series_length = as_factor(series_length)) %>% 
  ggplot(
    aes(x = Year)
  ) +
  facet_wrap(
    ~series_length,
    nrow = 6,
    ncol = 3,
    scales = "free"
  ) +
  geom_line(
    aes(
      y = value,
      color = variable
    )
  ) +
  scale_y_log10() +
  theme(
    legend.position = "none"
  )
  


abc <- tmelt %>% 
  group_by(variable) %>% 
  mutate(series_length = 43-  value %>% is.na %>% sum) %>% 
  arrange(desc(series_length)) %>% 
  mutate(series_length = as_factor(series_length)) 

tmelt %>% 
  ggplot(
    aes(
      x = Year,
      y = value,
      colour = variable,
      group = variable
    )
  ) + 
  geom_line(
    alpha = .8
  ) + 
  scale_y_log10() +
  coord_polar() +
  scale_color_viridis_d(
    option = "cividis"
  ) +
  labs(
    title = "Tourism Time Series: Everything All At Once",
    y = expression(log[10](Value))
  ) +
  theme(
    legend.position = "none"
  )
