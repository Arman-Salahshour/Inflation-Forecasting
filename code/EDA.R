# In pdfs/EDA.pdf you can find the notebook of this code, with all of its figures and tables
# Run it from the project folder, after code/Preprocessing.R

library(tidyverse)

# Step 1: look at the target

# 12-month inflation over time, with the 2021-2023 surge shaded and the 2% target as a dashed line
inflation_plot = ggplot(dataset, aes(x=date, y=inflation_12m)) +
  annotate("rect", xmin=ym("2021-01"), xmax=ym("2023-12"), ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_hline(yintercept=2, linetype="dashed", color="blue") +
  geom_line()+
  labs(title="Inflation Rate Over Time", x="Date", y="12-Month Inflation Rate") +
  theme_minimal()

inflation_plot
ggsave("images/inflation_rate_over_time.png",inflation_plot, width=8, height=4, dpi=300)

# Headline and core inflation in one plot, long format gives one line per series
dataset_long = dataset |>
  pivot_longer(cols = c(core_12m, inflation_12m), names_to = "series", values_to = "value")

core_inflation_plot = ggplot(dataset_long, aes(x=date, y=value, color=series)) +
  geom_line() +
  labs(title="Core vs Overall Inflation Rate", x="Date", y="Inflation Rate") +
  theme_minimal() +
  scale_color_manual(values=c("core_12m"="blue", "inflation_12m"="red"), labels=c("Core Inflation", "Overall Inflation")) +
  theme(legend.title=element_blank())

core_inflation_plot
ggsave("images/core_vs_overall_inflation.png", core_inflation_plot, width=8, height=4, dpi=300)

# Mean, sd, min and max of both series in the three periods
summary_dataset = dataset_long |>
  mutate(
    period = case_when(
      year(date) <= 2014 ~ "2007-2014",
      year(date) <= 2020 ~ "2015-2020",
      TRUE ~ "2021-2026"
    )
  ) |>
  group_by(series, period) |>
  summarise(mean=mean(value), sd=sd(value), min=min(value), max=max(value))

print(summary_dataset)


# Step 2: how much inflation depends on its own past

# Correlation with last month and with the same month last year
cor(dataset$inflation_12m, lag(dataset$inflation_12m, 1), use="complete.obs")
cor(dataset$inflation_12m, lag(dataset$inflation_12m, 12), use="complete.obs")

# Autocorrelation and partial autocorrelation up to two years back
a = acf(dataset$inflation_12m, lag.max = 24)

p = pacf(dataset$inflation_12m, lag.max = 24)

# Put the values in tables for ggplot, lag 0 is dropped from the ACF because it is always 1
acf_table = tibble(lag=a$lag[-1], acf=a$acf[-1])

pacf_table = tibble(lag=p$lag, acf=p$acf)

# 95% band, values inside it are not different from zero
band = 1.96/sqrt(length(dataset$inflation_12m))

acf_plot = ggplot(acf_table, aes(x=lag, y=acf))+
  geom_line()+
  geom_hline(yintercept=c(-band, band), linetype="dashed")+
  labs(title="Autocorrelation of Inflation Rate", x="Lag", y="Autocorrelation")+
  theme_minimal()

acf_plot
ggsave("images/autocorrelation_inflation_rate.png", acf_plot, width=8, height=4, dpi=300)

pacf_plot = ggplot(pacf_table, aes(x=lag, y=acf))+
  geom_line()+
  geom_hline(yintercept=c(-band, band), linetype="dashed")+
  labs(title="Partial Autocorrelation of Inflation Rate", x="Lag", y="Partial Autocorrelation")+
  theme_minimal()

pacf_plot
ggsave("images/partial_autocorrelation_inflation_rate.png", pacf_plot, width=8, height=4, dpi=300)


# BIC of AR models with 1 to 13 lags, lower is better
arima_bic = tibble(p=1:13) |>
  mutate(bic = map_dbl(p, \(k) BIC(arima(dataset$inflation_12m, order=c(k, 0, 0)))))

arima_bic


# Step 3: stationarity
# install.packages("tseries")
library(tseries)


# Round 1: the target and the predictors in levels
predictors = dataset |>
  select(inflation_12m, core_12m,
         unemp, krone_i44, policy_rate, eur_nok, usd_nok, rate_3m, yield_10y, term_spread,
         cpi_elec, credit_growth, retail_volume, brent_usd, us_cpi, hicp_ea)

# ADF tests for a unit root, KPSS tests for stationarity, so they check each other
test_stationary = function(x){
  tibble(
    adf_p = adf.test(x)$p.value,
    kpss_p = kpss.test(x, null="Level")$p.value
  )
}

# Run both tests on every column and decide, if the two tests disagree it is unclear
stationary_tests = function(df){
  df |>
    map(test_stationary) |>
    list_rbind(names_to = 'series') |>
    mutate(
      decision = case_when(
        adf_p < 0.05 & kpss_p > 0.05 ~ 'stationary',
        adf_p > 0.05 & kpss_p < 0.05 ~ 'non-stationary',
        TRUE ~ 'unclear'
      )
    )
}



# Round 2: the same tests after the 12-month changes
transformed_predictors = dataset |> select(starts_with("dl12_"), starts_with("d12_"))

stationary_table = bind_rows(
  stationary_tests(predictors) |> mutate(version = "level"),
  stationary_tests(transformed_predictors) |> mutate(version = "transformed")
)

print(stationary_table, n=Inf)


# The krone index before and after the transformation, one panel each
krone_long = dataset |>
  select(date, krone_i44, dl12_krone_i44) |>
  pivot_longer(cols = c(krone_i44, dl12_krone_i44), names_to = "series", values_to = "value")

krone_plot = ggplot(krone_long, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~series, scales="free_y", ncol=1,
             labeller=as_labeller(c(krone_i44="Level (index)", dl12_krone_i44="12-month log change (per cent)"))) +
  labs(title="Import-Weighted Krone Index: Level vs 12-Month Change", x="Date", y=NULL) +
  theme_minimal()

krone_plot
ggsave("images/krone_level_vs_change.png", krone_plot, width=8, height=5, dpi=300)


# Step 4: which predictors move with future inflation

# Predictors that passed step 3, with both versions of credit growth to choose between them
all_predictors = c(
  "dl12_krone_i44", "dl12_eur_nok", "dl12_usd_nok", "dl12_cpi_elec", "dl12_brent_usd",
  "dl12_retail_volume", "dl12_us_cpi",
  "d12_unemp", "d12_policy_rate", "d12_rate_3m", "d12_yield_10y",
  "term_spread", "hicp_ea", "credit_growth", "d12_credit_growth"
)

# Correlation of each predictor today with the target h months later
# cor_level uses the future value, cor_change uses the future change, the part a random walk misses
correlation_table = function(df, predictors, target){
  y = df[[target]]
  expand_grid(series=predictors, h=c(1, 3, 6, 12)) |>
    mutate(
      cor_level = map2_dbl(series, h, \(s, k) cor(df[[s]], lead(y, k), use="complete.obs")),
      cor_change = map2_dbl(series, h, \(s, k) cor(df[[s]], lead(y, k) - y, use="complete.obs"))
    )
}

# Decisions are made on 2007-2014 only, since those months are never forecast
# We filter before lead() so it cannot reach into 2015 and later
train_eda = dataset |> filter(date <= ym("2014-12"))

correlation_grid = bind_rows(
  correlation_table(train_eda, all_predictors, "inflation_12m") |> mutate(sample="2007-2014 (training)"),
  correlation_table(dataset, all_predictors, "inflation_12m") |> mutate(sample="2007-2026 (full)")
)

print(correlation_grid, n=Inf)


# Heatmap of the correlations, training sample next to the full sample
plot_correlation = function(cor_data, measure, title){
  ggplot(cor_data, aes(x=factor(h), y=series, fill=.data[[measure]])) +
    geom_tile() +
    geom_text(aes(label=round(.data[[measure]], 2)), size=3) +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, limits=c(-1, 1)) +
    facet_wrap(~sample) +
    labs(title=title, x="Horizon (months)", y=NULL, fill="Correlation") +
    theme_minimal()
}

change_plot = plot_correlation(correlation_grid, "cor_change", "Correlation with the future change in headline inflation")
change_plot
ggsave("images/correlation_change_headline.png", change_plot, width=10, height=6, dpi=300)

level_plot = plot_correlation(correlation_grid, "cor_level", "Correlation with the future level of headline inflation")
level_plot
ggsave("images/correlation_level_headline.png", level_plot, width=10, height=6, dpi=300)


# Same heatmap for core inflation
correlation_grid_core = bind_rows(
  correlation_table(train_eda, all_predictors, "core_12m") |> mutate(sample="2007-2014 (training)"),
  correlation_table(dataset, all_predictors, "core_12m") |> mutate(sample="2007-2026 (full)")
)

core_change_plot = plot_correlation(correlation_grid_core, "cor_change", "Correlation with the future change in core inflation")
core_change_plot
ggsave("images/correlation_change_core.png", core_change_plot, width=10, height=6, dpi=300)


# Credit growth in level or as a 12-month change, compared on the training window
correlation_grid |>
  filter(series %in% c("credit_growth", "d12_credit_growth"), sample == "2007-2014 (training)") |>
  select(series, h, cor_level, cor_change)




# Step 5: how unusual the surge was

# Range, mean and sd of a series in the reference period
reference_stats = function(x){
  tibble(
    ref_min = min(x),
    ref_max = max(x),
    ref_mean = mean(x),
    ref_sd = sqrt(var(x))
  )
}

# Share of months more than 3 sd away from the reference mean
share_extreme_z = function(x, ref_mean, ref_sd){
  abs_z = abs((x - ref_mean)/ref_sd)
  mean(abs_z>3)
}

# Share of months outside the reference range
share_outside_range = function(x, ref_min, ref_max){
  mean(x < ref_min | x > ref_max)
}

# The 14 predictors that go into the models
model_predictors = c(
  "dl12_krone_i44", "dl12_eur_nok", "dl12_usd_nok", "dl12_cpi_elec", "dl12_brent_usd",
  "dl12_retail_volume", "dl12_us_cpi",
  "d12_unemp", "d12_policy_rate", "d12_rate_3m", "d12_yield_10y",
  "term_spread", "hicp_ea", "credit_growth"
)

# Reference period: everything before the surge (2007-2020)
pre_surge = dataset |>
  filter(date <= ym('2020-12')) |>
  select(c(model_predictors, 'inflation_12m'))

pre_surge_stats = pre_surge |>
  map(reference_stats) |>
  list_rbind(names_to = 'series')


surge = dataset |>
  filter(date > ym('2020-12')) |>
  select(c(model_predictors, 'inflation_12m'))

# For each series, how often the surge months fall far from or outside the pre-surge values
pre_surge_stats |>
  mutate(
    share_z_above_3=pmap_dbl(list(series, ref_mean, ref_sd), \(name, m, s) share_extreme_z(surge[[name]], m, s)),
    share_outside=pmap_dbl(list(series, ref_min, ref_max), \(name, lo, hi) share_outside_range(surge[[name]], lo, hi))
  )


# install.packages("slider")
# install.packages("strucchange")
library(slider)
library(strucchange)

# Breaks in the mean of inflation, each segment at least 2 years long
bp = breakpoints(inflation_12m ~ 1, data=dataset, h=24)
summary(bp)
dataset$date[bp$breakpoints]


# Rolling correlations: do the relationships stay the same over time?
rolling_predictors = c("dl12_cpi_elec", "d12_unemp", "dl12_retail_volume", "dl12_krone_i44")

# Change of inflation over the next 12 months
# The last 12 months have no future yet, so they are dropped
rolling_data = dataset |>
  arrange(date) |>
  mutate(future_change_12 = lead(inflation_12m, 12) - inflation_12m) |>
  filter(!is.na(future_change_12))

# Correlation over the last 60 months, dated at the end of the window
rolling_cor = function(x, y){
  slide2_dbl(x, y, \(a, b) cor(a, b), .before=59, .complete=TRUE)
}

rolling_table = rolling_data |>
  mutate(across(all_of(rolling_predictors), \(x) rolling_cor(x, future_change_12))) |>
  select(date, all_of(rolling_predictors)) |>
  pivot_longer(cols=all_of(rolling_predictors), names_to="series", values_to="rolling_cor") |>
  filter(!is.na(rolling_cor))

rolling_plot = ggplot(rolling_table, aes(x=date, y=rolling_cor)) +
  geom_hline(yintercept=0, color="grey50") +
  geom_vline(xintercept=ym("2021-01"), linetype="dashed", color="red") +
  geom_line() +
  facet_wrap(~series, ncol=2) +
  coord_cartesian(ylim=c(-1, 1)) +
  labs(title="Rolling correlation with the future change in inflation",
       subtitle="60-month window, change over the next 12 months", x=NULL, y="Correlation") +
  theme_minimal()

rolling_plot
ggsave("images/rolling_correlation.png", rolling_plot, width=10, height=6, dpi=300)

# Rolling correlation at the end of training, just before the surge, and at the latest date
rolling_table |>
  filter(date %in% ym(c("2014-12", "2020-12", "2025-08"))) |>
  pivot_wider(names_from=date, values_from=rolling_cor)




