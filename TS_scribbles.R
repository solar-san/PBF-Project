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

naive_train

fit_naive_1 <- train %>% 
  select(Y1) %>% 
  model(NAIVE(Y1))

augment(fit_naive_1) %>% na.omit()

accuracy(fit_naive_1)
  
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


### Plotting only the IQR

# Applying a filter to obtain only the _Inter-Quartile Range_ (IQR) in our plots provides a visual representation of the central 50% of the data points that cluster around the median. This visualization enhances our ability to interpret the observed variability as depicted in @fig-every.
# 
# It becomes apparent that there are two distinct trend-cycles and an increasing degree of variability within the collected data. This variation could be attributed to the absence of long-running time series. A consistent overall trend persists across all years, unaffected by the declining phase that spans roughly from 1975 to 1985, during which the widest range of values is also observed. Nevertheless, the upward trend remains robust and consistent throughout.

#| fig-cap: "The following plot represents the position indexes given by the mean and the quartile, computed by considering all the series as observations and discarding missing observations."
#| fig-width: 12
#| fig-height: 6
tmelt %>% 
  #  mutate(
  #    Time_Interval = cut(
  #      Year,
  #      breaks = c(1964, 1975, 1985, 1995, 2003, 2008)
  #    )
  #  ) %>%  
  # group_by(Time_Interval) %>% 
  mutate(
    median_value = median(Value, na.rm = T),
    mean_value = mean(Value, na.rm = T),
    q_0.25 = quantile(Value, probs = .25, na.rm = T),
    q_0.75 = quantile(Value, probs = .75, na.rm = T)
  ) %>% 
  ggplot(
    aes(
      x = Year
    )
  ) + 
  geom_line(
    aes(
      y = mean_value,
      color = viridisLite::rocket(1, begin = 1),
    ),
    linewidth = 1,
    linetype = "dotted"
  ) + 
  geom_line(
    aes(
      y = median_value,
      color = viridisLite::rocket(1, begin = .5)
    ),
    linewidth = 1
  ) + 
  geom_line(
    aes(
      y = q_0.25,
      color = viridisLite::rocket(1, begin = .25)
    ),
    linewidth = 1
  ) + 
  geom_line(
    aes(
      y = q_0.75,
      color = viridisLite::rocket(1, begin = .75)
    ),
    linewidth = 1
  ) + 
  geom_ribbon(
    aes(
      ymin = q_0.25,
      ymax = q_0.75,
    ),
    fill = "grey95",
    alpha = .5
  ) +
  scale_y_log10()  +
  scale_color_viridis_d(
    labels = c(
      expression(q[0.25]),
      expression(q[0.50]),
      expression(q[0.75]),
      expression(mu)
    ),
    option = "rocket",
    direction = -1,
    end = .9
  ) +
  labs(
    title = "Tourism Time Series: Quartiles and Mean",
    y = expression(log[10](Value)),
    colour = "Index"
  )

tmelt %>% 
  filter(
    Identifier == "Y1" |
      Identifier == "Y2" |
      Identifier == "Y3" |
      Identifier == "Y4" |
      Identifier == "Y5"
  ) %>% 
  filter(Year > 2003) %>% 
  is.na() %>% 
  sum
