# In pdfs/Modelling.pdf you can find the notebook of this code, with all of its figures and tables
# Run it from the project folder, after code/Preprocessing.R

# The 14 predictors we chose in the EDA
model_predictors = c(
  "dl12_krone_i44", "dl12_eur_nok", "dl12_usd_nok", "dl12_cpi_elec", "dl12_brent_usd",
  "dl12_retail_volume", "dl12_us_cpi",
  "d12_unemp", "d12_policy_rate", "d12_rate_3m", "d12_yield_10y",
  "term_spread", "hicp_ea", "credit_growth"
)

# The small set of predictors for the OLS benchmark
ols_predictors = c("d12_unemp", "dl12_krone_i44", "d12_policy_rate")

# Lags of inflation, all features, and the target for each horizon
lags_name = paste0('inflation_12m_lag', 0:12)
features = c(model_predictors, lags_name)
targets = paste0('inflation_12m_lead', c(1, 3, 6, 12))


# Try the models on one forecast date and one horizon
origin = ym("2015-01")
h = 12

# Train only on rows whose target was already known at the forecast date
train = ml_dataset |> filter(date <= origin %m-% months(h))
test = ml_dataset |> filter(date==origin)

target = paste0('inflation_12m_lead', h)

x_train = train[features]
y_train = train[[target]]

x_test = test[features]
y_test = test[[target]]


# Pick the number of inflation lags with the lowest BIC
choose_p = function(train, target, max_p=13, verbose=FALSE){
  bic_table = tibble(p=1:max_p) |>
    mutate(bic=map_dbl(p, \(k) BIC(lm(reformulate(lags_name[1:k], response=target), data=train))))
  
  if (verbose==TRUE) print(bic_table, n=Inf)
  
  bic_table$p[which.min(bic_table$bic)]
}

best_p = choose_p(train, target, 13, verbose=TRUE)
best_p

# AR model: only past inflation
forecast_ar = function(train, target, new_row, max_p=13){
  p = choose_p(train, target, max_p)
  f = reformulate(lags_name[1:p], response=target)
  fit = lm(f, data=train)
  predict(fit, newdata=new_row)
}

# OLS model: the same lags as AR, plus three economic predictors
forecast_ols = function(train, target, new_row, max_p=13){
  p = choose_p(train, target, max_p)
  f = reformulate(c(lags_name[1:p], ols_predictors), response=target)
  fit = lm(f, data=train)
  predict(fit, newdata=new_row)
}

# Random walk: inflation stays where it is today
forecast_rw = function(train, target, new_row){
  new_row$inflation_12m_lag0
}


# Forecast the test row with each benchmark
ar_forecast = forecast_ar(train, target, test)
ols_forecast = forecast_ols(train, target, test)
rw_forecast = forecast_rw(train, target, test)

# Compare the forecasts with the actual value
tibble(ar=ar_forecast, rw=rw_forecast, ols=ols_forecast, actual=y_test)



# Every forecast date and horizon whose actual value is already in the data
date_grid = expand_grid(origin=seq(ym("2015-01"), ym("2026-08"), by='month'), h=c(1, 3, 6, 12)) |>
  filter(origin <= ym("2026-08") %m-% months(h))

# One forecast date and one horizon: train, forecast with each benchmark, keep the actual value
one_round_prediction = function(origin, h){
  
  train = ml_dataset |> filter(date <= origin %m-% months(h))
  test = ml_dataset |> filter(date==origin)
  target = paste0('inflation_12m_lead', h)
  
  ar_forecast = forecast_ar(train, target, test)
  ols_forecast = forecast_ols(train, target, test)
  rw_forecast = forecast_rw(train, target, test)
  
  tibble(origin=origin, h=h, target_date=origin %m+% months(h),
         ar=ar_forecast, rw=rw_forecast, ols=ols_forecast, actual=test[[target]])
}

results = map2(date_grid$origin, date_grid$h, one_round_prediction) |> list_rbind()

# Save the forecasts 
dir.create("results", showWarnings=FALSE)
write_csv(results, "results/benchmark_forecasts.csv")

# Quick checks
count(results, h)
results |> filter(origin == ym("2015-01"), h == 12)

# print(results, n=Inf)


# install.packages("glmnet")
library(glmnet)

# LASSO, ridge and elastic net differ only in alpha
# glmnet scales the features with the training data itself, so we do not scale them before
# penalty has one weight for each feature: 1 is the normal penalty, 0 means the feature is never shrunk
forecast_glmnet = function(train, target, new_row, alpha, penalty=rep(1, length(features))){
  x_train = as.matrix(train[features])
  y_train = train[[target]]
  
  x_test = as.matrix(new_row[features])
  
  # Five folds in time order, so the cross validation does not mix past and future at random
  n = nrow(train)
  folds = ceiling(seq_len(n) / (n / 5))
  cv_fit = cv.glmnet(x_train, y_train, alpha=alpha, foldid=folds, penalty.factor=penalty)
  
  as.numeric(predict(cv_fit, newx=x_test, s="lambda.min"))
}

forecast_lasso = function(train, target, new_row) forecast_glmnet(train, target, new_row, alpha=1)
forecast_ridge = function(train, target, new_row) forecast_glmnet(train, target, new_row, alpha=0)
forecast_enet = function(train, target, new_row) forecast_glmnet(train, target, new_row, alpha=0.5)


# Test on the single date first, and see which variables LASSO keeps
forecast_lasso(train, target, test)
forecast_ridge(train, target, test)
forecast_enet(train, target, test)



x_checks = as.matrix(train[features])
y_checks = train[[target]]
n = nrow(x_checks)

folds = ceiling(seq_len(n)/ (n/5))

coef(cv.glmnet(x_checks, y_checks, foldid=folds, alpha=1), s="lambda.min")

# One forecast date and one horizon: train, forecast with each model, keep the actual value
one_round_prediction = function(origin, h){
  
  train = ml_dataset |> filter(date <= origin %m-% months(h))
  test = ml_dataset |> filter(date==origin)
  target = paste0('inflation_12m_lead', h)
  
  ar_forecast = forecast_ar(train, target, test)
  ols_forecast = forecast_ols(train, target, test)
  rw_forecast = forecast_rw(train, target, test)
  lasso_forecast = forecast_lasso(train, target, test)
  ridge_forecast = forecast_ridge(train, target, test)
  enet_forecast = forecast_enet(train, target, test)
  
  tibble(origin=origin, h=h, target_date=origin %m+% months(h),
         ar=ar_forecast, rw=rw_forecast, ols=ols_forecast,
         lasso=lasso_forecast, ridge=ridge_forecast, enet=enet_forecast,
         actual=test[[target]])
}

results = map2(date_grid$origin, date_grid$h, one_round_prediction) |> list_rbind()

# Save the forecasts 
write_csv(results, "results/forecasts.csv")

# Quick checks
count(results, h)
results |> filter(origin == ym("2015-01"), h == 12)





# The same three models, but today's inflation is never shrunk
# So the simplest model is close to AR(1) and not just the mean,
# and the penalty only decides if the other lags and the 14 predictors add something
keep_lag0 = ifelse(features == "inflation_12m_lag0", 0, 1)

forecast_lasso_l0 = function(train, target, new_row) forecast_glmnet(train, target, new_row, alpha=1, penalty=keep_lag0)
forecast_ridge_l0 = function(train, target, new_row) forecast_glmnet(train, target, new_row, alpha=0, penalty=keep_lag0)
forecast_enet_l0 = function(train, target, new_row) forecast_glmnet(train, target, new_row, alpha=0.5, penalty=keep_lag0)


# Check on the single date: TRUE means cross validation picked the model with all coefficients at zero
cv_old = cv.glmnet(x_checks, y_checks, foldid=folds, alpha=1)
cv_new = cv.glmnet(x_checks, y_checks, foldid=folds, alpha=1, penalty.factor=keep_lag0)

cv_old$lambda.min == max(cv_old$lambda)
cv_new$lambda.min == max(cv_new$lambda)

# lag0 should always be in the model now
coef(cv_new, s="lambda.min")


# Only the three new models, the benchmarks are already in results
one_round_prediction_l0 = function(origin, h){
  
  train = ml_dataset |> filter(date <= origin %m-% months(h))
  test = ml_dataset |> filter(date==origin)
  target = paste0('inflation_12m_lead', h)
  
  tibble(origin=origin, h=h, target_date=origin %m+% months(h),
         lasso=forecast_lasso_l0(train, target, test),
         ridge=forecast_ridge_l0(train, target, test),
         enet=forecast_enet_l0(train, target, test),
         actual=test[[target]])
}

results_l0 = map2(date_grid$origin, date_grid$h, one_round_prediction_l0) |> list_rbind()

# Save in a separate file to compare with the first version
write_csv(results_l0, "results/forecasts_keep_lag0.csv")


# Compare the two versions: RMSE of each model and horizon, with AR as the reference
rmse = function(forecast, actual) sqrt(mean((forecast - actual)^2))

ar_rmse = results |>
  group_by(h) |>
  summarise(ar=rmse(ar, actual))

comparison = bind_rows(
  results |> select(h, lasso, ridge, enet, actual) |> mutate(version="old"),
  results_l0 |> select(h, lasso, ridge, enet, actual) |> mutate(version="keep_lag0")
) |>
  pivot_longer(cols=c(lasso, ridge, enet), names_to="model", values_to="forecast") |>
  group_by(version, model, h) |>
  summarise(rmse=rmse(forecast, actual), .groups="drop") |>
  pivot_wider(names_from=version, values_from=rmse) |>
  left_join(ar_rmse, by="h") |>
  arrange(model, h)

print(comparison, n=Inf)



# install.packages("ranger")
library(ranger)

# Random forest
# 500 trees, each split tries 9 of the 27 features (p/3)
# Fixed settings in every window, the out of bag error is not reliable with overlapping targets
# The seed makes each window give the same forecast every time we run it
forecast_rf = function(train, target, new_row){
  fit = ranger(x=train[features], y=train[[target]],
               num.trees=500, mtry=9, min.node.size=5, seed=1)
  
  predict(fit, data=new_row[features])$predictions
}



# Test on the single date first
forecast_rf(train, target, test)

# Run it again, it should give exactly the same number
forecast_rf(train, target, test)

# The forest can not forecast above the highest target it has seen in training
max(train[[target]])

# Which features the forest relies on, only for this one window
rf_check = ranger(x=train[features], y=train[[target]],
                  num.trees=500, mtry=9, min.node.size=5, seed=1, importance="permutation")
sort(rf_check$variable.importance, decreasing=TRUE)


# Only the random forest, the other models are already saved
one_round_prediction_rf = function(origin, h){
  
  train = ml_dataset |> filter(date <= origin %m-% months(h))
  test = ml_dataset |> filter(date==origin)
  target = paste0('inflation_12m_lead', h)
  
  tibble(origin=origin, h=h, target_date=origin %m+% months(h),
         rf=forecast_rf(train, target, test),
         train_max=max(train[[target]]),
         actual=test[[target]])
}

results_rf = map2(date_grid$origin, date_grid$h, one_round_prediction_rf) |> list_rbind()

# Save in a separate file
write_csv(results_rf, "results/forecasts_rf.csv")


# Checks: no forecast above the training maximum, and no missing values
results_rf |> summarise(above_max=sum(rf > train_max), missing=sum(is.na(rf)))
count(results_rf, h)

# RMSE of the random forest next to AR
results_rf |>
  group_by(h) |>
  summarise(rf=rmse(rf, actual)) |>
  left_join(ar_rmse, by="h") |>
  mutate(rf_vs_ar=rf / ar)



# install.packages("forecast")
library(forecast)


# All forecasts in one table: benchmarks from the first run, glmnet from the keep_lag0 run, and the random forest
benchmarks = read_csv("results/forecasts.csv", show_col_types=FALSE) |>
  select(origin, h, target_date, actual, ar, rw, ols)

glmnet_l0 = read_csv("results/forecasts_keep_lag0.csv", show_col_types=FALSE) |>
  select(origin, h, lasso, ridge, enet)

rf_forecasts = read_csv("results/forecasts_rf.csv", show_col_types=FALSE) |>
  select(origin, h, rf)

all_forecasts = benchmarks |>
  left_join(glmnet_l0, by=c("origin", "h")) |>
  left_join(rf_forecasts, by=c("origin", "h"))

# Should be 538 rows and no missing values
nrow(all_forecasts)
all_forecasts |> summarise(across(everything(), ~sum(is.na(.x))))

model_names = c("ar", "rw", "ols", "lasso", "ridge", "enet", "rf")

# One row per forecast date, horizon and model
# The period is set by the month being forecast, not the month the forecast was made
to_long = function(forecasts){
  forecasts |>
    pivot_longer(cols=all_of(model_names), names_to="model", values_to="forecast") |>
    mutate(
      error=actual - forecast,
      period=if_else(target_date < ym("2021-01"), "2015-2020", "2021-2026")
    )
}

forecasts_long = to_long(all_forecasts)

# The same rows once more with one label for the whole sample, so one summarise gives all three periods
forecasts_eval = bind_rows(
  forecasts_long |> mutate(period="2015-2026"),
  forecasts_long
)


# RMSE and MAE, and both relative to AR in the same horizon and period
measure_accuracy = function(forecasts){
  forecasts |>
    group_by(period, h, model) |>
    summarise(n=n(), rmse=sqrt(mean(error^2)), mae=mean(abs(error)), .groups="drop") |>
    group_by(period, h) |>
    mutate(rel_rmse=rmse / rmse[model == "ar"], rel_mae=mae / mae[model == "ar"]) |>
    ungroup()
}

accuracy = measure_accuracy(forecasts_eval)

# Table for the paper: AR in levels, every other model relative to AR
relative_table = function(accuracy, rel_col, level_col){
  ar_level = accuracy |>
    filter(model == "ar") |>
    select(period, h, ar=all_of(level_col))
  
  accuracy |>
    filter(model != "ar") |>
    select(period, h, model, value=all_of(rel_col)) |>
    pivot_wider(names_from=model, values_from=value) |>
    left_join(ar_level, by=c("period", "h")) |>
    relocate(ar, .after=h) |>
    mutate(across(where(is.numeric) & !h, \(x) round(x, 2))) |>
    arrange(period, h)
}

rmse_table = relative_table(accuracy, "rel_rmse", "rmse")
mae_table = relative_table(accuracy, "rel_mae", "mae")

print(rmse_table, n=Inf)
print(mae_table, n=Inf)

write_csv(rmse_table, "results/table_rmse.csv")
write_csv(mae_table, "results/table_mae.csv")


# Diebold-Mariano tests against AR

# Errors side by side, so each model is compared with AR on exactly the same forecast dates
errors_wide = forecasts_eval |>
  select(period, h, origin, model, error) |>
  pivot_wider(names_from=model, values_from=error) |>
  arrange(period, h, origin)

# One model against AR with squared errors
# dm.test includes the small sample correction of Harvey, Leybourne and Newbold
# The bartlett variance can not become negative, which can happen with few long horizon forecasts
# A negative statistic means the model has smaller errors than AR
dm_vs_ar = function(p, horizon, m){
  d = errors_wide |> filter(period == p, h == horizon)
  test = dm.test(d[[m]], d$ar, h=horizon, power=2, varestimator="bartlett")
  tibble(dm_stat=unname(test$statistic), p_value=test$p.value)
}

# If a test fails for one cell, keep NA and go on
safe_dm = possibly(dm_vs_ar, otherwise=tibble(dm_stat=NA_real_, p_value=NA_real_))

dm_results = expand_grid(period=c("2015-2026", "2015-2020", "2021-2026"), h=c(1, 3, 6, 12),
                         model=setdiff(model_names, "ar")) |>
  mutate(test=pmap(list(period, h, model), safe_dm)) |>
  unnest(test)

print(dm_results, n=Inf)

# Relative RMSE with stars from the DM test: * 10%, ** 5%, *** 1%
dm_table = dm_results |>
  left_join(accuracy |> select(period, h, model, rel_rmse), by=c("period", "h", "model")) |>
  mutate(
    stars=case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", p_value < 0.10 ~ "*", TRUE ~ ""),
    cell=paste0(sprintf("%.2f", rel_rmse), stars)
  ) |>
  select(period, h, model, cell) |>
  pivot_wider(names_from=model, values_from=cell) |>
  arrange(period, h)

print(dm_table, n=Inf)
write_csv(dm_table, "results/table_rmse_dm.csv")


# Figures

# Relative RMSE by horizon, one panel per period
rel_plot = accuracy |>
  filter(model != "ar") |>
  mutate(period=factor(period, levels=c("2015-2020", "2021-2026", "2015-2026"))) |>
  ggplot(aes(x=factor(h), y=rel_rmse, color=model, group=model)) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_line() +
  geom_point() +
  scale_color_discrete(limits=c("rw", "ols", "lasso", "ridge", "enet", "rf"),
                       labels=c("Random walk", "OLS", "LASSO", "Ridge", "Elastic net", "Random forest")) +
  facet_wrap(~period) +
  labs(title="Forecast accuracy relative to the AR benchmark",
       subtitle="RMSE ratio, below 1 means better than AR",
       x="Horizon (months)", y="RMSE relative to AR", color=NULL) +
  theme_minimal()

rel_plot
ggsave("images/relative_rmse.png", rel_plot, width=10, height=5, dpi=300)

# Forecasts against the actual values over time, 1 and 12 months ahead
path_plot = forecasts_long |>
  filter(h %in% c(1, 12), model %in% c("ar", "rw", "lasso", "rf")) |>
  ggplot(aes(x=target_date)) +
  geom_line(aes(y=actual), color="black", linewidth=0.8) +
  geom_line(aes(y=forecast, color=model)) +
  facet_wrap(~h, ncol=1, labeller=as_labeller(c(`1`="1 month ahead", `12`="12 months ahead"))) +
  labs(title="Forecasts and actual inflation", subtitle="Black line is the actual 12-month inflation",
       x=NULL, y="12-month inflation (per cent)", color=NULL) +
  theme_minimal()

path_plot
ggsave("images/forecast_paths.png", path_plot, width=10, height=7, dpi=300)


# which variables LASSO selects over time

# Refit LASSO (keep_lag0 version) for one forecast date and horizon, and keep all coefficients
lasso_coefs = function(origin, h){
  train = ml_dataset |> filter(date <= origin %m-% months(h))
  target = paste0('inflation_12m_lead', h)
  
  x_train = as.matrix(train[features])
  y_train = train[[target]]
  
  n = nrow(train)
  folds = ceiling(seq_len(n) / (n / 5))
  cv_fit = cv.glmnet(x_train, y_train, alpha=1, foldid=folds, penalty.factor=keep_lag0)
  
  b = coef(cv_fit, s="lambda.min")
  tibble(origin=origin, h=h, feature=rownames(b), coef=as.numeric(as.matrix(b))) |>
    filter(feature != "(Intercept)")
}

lasso_path = map2(date_grid$origin, date_grid$h, lasso_coefs) |>
  list_rbind() |>
  mutate(
    selected=coef != 0,
    target_date=origin %m+% months(h),
    period=if_else(target_date < ym("2021-01"), "2015-2020", "2021-2026")
  )

write_csv(lasso_path, "results/lasso_coefficients.csv")

# Checks: 27 features in each round, and lag0 always kept because it is not penalized
nrow(lasso_path) == 538 * 27
lasso_path |> filter(feature == "inflation_12m_lag0") |> summarise(always_kept=all(selected))


# How often each variable is selected, for each horizon
selection_share = lasso_path |>
  group_by(feature, h) |>
  summarise(share=mean(selected), .groups="drop") |>
  pivot_wider(names_from=h, values_from=share, names_prefix="h") |>
  mutate(across(starts_with("h"), \(x) round(x, 2))) |>
  arrange(desc(h12))

print(selection_share, n=Inf)
write_csv(selection_share, "results/lasso_selection_share.csv")

# The same before and after the surge, all horizons together
selection_change = lasso_path |>
  group_by(feature, period) |>
  summarise(share=mean(selected), .groups="drop") |>
  pivot_wider(names_from=period, values_from=share) |>
  mutate(change=`2021-2026` - `2015-2020`) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  arrange(desc(abs(change)))

print(selection_change, n=Inf)


# Figures
horizon_labels = as_labeller(c(`1`="h = 1", `3`="h = 3", `6`="h = 6", `12`="h = 12"))

# Sign of each coefficient at every forecast date: red positive, blue negative, grey not selected
sign_plot = lasso_path |>
  mutate(
    sign=case_when(coef > 0 ~ "positive", coef < 0 ~ "negative", TRUE ~ "not selected"),
    feature=factor(feature, levels=rev(features))
  ) |>
  ggplot(aes(x=origin, y=feature, fill=sign)) +
  geom_tile() +
  geom_vline(xintercept=ym("2021-01"), linetype="dashed") +
  scale_fill_manual(values=c(positive="firebrick", negative="steelblue", `not selected`="grey92")) +
  facet_wrap(~h, nrow=1, labeller=horizon_labels) +
  labs(title="Variables selected by LASSO over time",
       subtitle="Sign of the coefficient at each forecast date",
       x="Forecast date", y=NULL, fill=NULL) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90))

sign_plot
ggsave("images/lasso_selection.png", sign_plot, width=14, height=7, dpi=300)

# Number of selected predictors and inflation lags at every forecast date
count_plot = lasso_path |>
  mutate(type=if_else(feature %in% model_predictors, "Predictors (out of 14)", "Inflation lags (out of 13)")) |>
  group_by(origin, h, type) |>
  summarise(n_selected=sum(selected), .groups="drop") |>
  ggplot(aes(x=origin, y=n_selected, color=type)) +
  geom_line() +
  geom_vline(xintercept=ym("2021-01"), linetype="dashed") +
  facet_wrap(~h, ncol=2, labeller=horizon_labels) +
  labs(title="How many variables LASSO keeps",
       x="Forecast date", y="Number selected", color=NULL) +
  theme_minimal()

count_plot
ggsave("images/lasso_count.png", count_plot, width=10, height=6, dpi=300)


# Robustness checks

# All seven models for one forecast date and one horizon, on the table given in data
# window is the number of training months to keep, Inf keeps all of them
one_round_all = function(origin, h, data, window=Inf){
  
  train = data |> filter(date <= origin %m-% months(h)) |> slice_tail(n=window)
  test = data |> filter(date==origin)
  target = paste0('inflation_12m_lead', h)
  
  tibble(origin=origin, h=h, target_date=origin %m+% months(h),
         ar=forecast_ar(train, target, test),
         rw=forecast_rw(train, target, test),
         ols=forecast_ols(train, target, test),
         lasso=forecast_lasso_l0(train, target, test),
         ridge=forecast_ridge_l0(train, target, test),
         enet=forecast_enet_l0(train, target, test),
         rf=forecast_rf(train, target, test),
         actual=test[[target]])
}

# RMSE and MAE for a table of forecasts, with the same steps as for headline inflation
accuracy_of = function(results){
  long = to_long(results)
  measure_accuracy(bind_rows(long |> mutate(period="2015-2026"), long))
}


# First check: core inflation (CPI-ATE) as the target

# The lags and targets of core inflation get the column names the model functions already use,
# so the same functions run on this table
# The first 12 rows have no lag 12, so the core sample starts in 2008-02
ml_core = ml_dataset |>
  select(date, all_of(model_predictors)) |>
  bind_cols(
    map(0:12, \(k) lag(ml_dataset$core_12m, k)) |> set_names(lags_name) |> as_tibble(),
    map(c(1, 3, 6, 12), \(k) lead(ml_dataset$core_12m, k)) |> set_names(targets) |> as_tibble()
  ) |>
  filter(!is.na(inflation_12m_lag12))

results_core = map2(date_grid$origin, date_grid$h, \(origin, h) one_round_all(origin, h, ml_core)) |>
  list_rbind()

write_csv(results_core, "results/forecasts_core.csv")

# Should be 538 rows and no missing values
nrow(results_core)
results_core |> summarise(across(everything(), ~sum(is.na(.x))))

core_table = relative_table(accuracy_of(results_core), "rel_rmse", "rmse")
print(core_table, n=Inf)
write_csv(core_table, "results/table_rmse_core.csv")


# Second check: a rolling window
# Every model is trained on the last 96 months (8 years) only, so the oldest data drop out over time
results_rolling = map2(date_grid$origin, date_grid$h, \(origin, h) one_round_all(origin, h, ml_dataset, window=96)) |>
  list_rbind()

write_csv(results_rolling, "results/forecasts_rolling.csv")

# Should be 538 rows and no missing values
nrow(results_rolling)
results_rolling |> summarise(across(everything(), ~sum(is.na(.x))))

# RMSE of the rolling forecasts relative to the AR model with the expanding window,
# so the numbers can be compared with the main table
# The ar column shows what AR itself loses when it forgets the old data
rolling_table = accuracy_of(results_rolling) |>
  left_join(accuracy |> filter(model == "ar") |> select(period, h, ar_expanding=rmse), by=c("period", "h")) |>
  mutate(ratio=round(rmse / ar_expanding, 2)) |>
  select(period, h, model, ratio) |>
  pivot_wider(names_from=model, values_from=ratio) |>
  arrange(period, h)

print(rolling_table, n=Inf)
write_csv(rolling_table, "results/table_rmse_rolling.csv")
