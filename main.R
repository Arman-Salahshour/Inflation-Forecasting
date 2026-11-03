# Data tyding and data wrangling

library(tidyverse)
# install.packages("eurostat")
library(eurostat) 


# Download Helpers
start_month = "2005-01"
end_month = "2026-08"

# Norges Bank: one series, given its dataset and key
get_norges_bank = function(dataset, key){
  # first we construct the needed url
  url = paste0(
    "https://data.norges-bank.no/api/data/",
    dataset,
    "/",key,
    "?format=csv&startPeriod=", start_month,   
    "&endPeriod=", end_month,
    "&locale=en"
  )
  
  # and read the dataset from the url then select the date and observed data, mutate and arrage the date column
  read_delim(url, delim = ';', show_col_types=FALSE) |> 
    select(date=TIME_PERIOD, value=OBS_VALUE) |>  
    mutate(date = ym(date))|> 
    arrange(date)
}


# SSB: one table, given its number and the query that selects the series
get_ssb = function(table, query) {
  url = paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/", table,
    "/data?outputFormat=csv&", query
  )
  
  # read the dataset, and change the format of the date, to have auniform format
  read_csv(url, show_col_types=FALSE) |>
    mutate(date=ym(str_replace(Tid, "M", "-")), .before=1) |>
    select(-Tid) |>
    filter(date <= ym(end_month))  
}

# FRED: one series, given its id
get_fred = function(series_id){
  # First we construct the needed url, FRED wants full dates (start: first day of start_month, end: last day of end_month)
  url = paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id,
    "&cosd=", start_month, "-01",
    "&coed=", ym(end_month) %m+% months(1) - days(1)
  )
  
  # FRED marks missing values with ".", and the column names have changed over the years, so we rename by position
  read_csv(url, na = c("", "."), show_col_types=FALSE) |> 
    rename(date=1, value=2) |> 
    mutate(date = as_date(date)) |> 
    arrange(date)
}


# Fetch the exchange rate information from the API and just keep two columns: date, observed_values
exr = get_norges_bank("EXR", "M.I44.NOK.SP") |> rename(i44_index=value)   

# Fetch the Policy rate information from the API and just keep two columns: date, observed_values
pr = get_norges_bank("IR", "M.KPRA.SD.R") |> rename(policy_rate_pct=value)   


# Fetch headline CPI and CPI-ATE: index, monthly change, 12-month change
cpi = get_ssb(
  "14706",
  "valueCodes[KPIavledetSerie]=KPI,KPI-JAE&valueCodes[ContentsCode]=*&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=KPIavledetSerie,ContentsCode"
) |>
  rename( 
    cpi_index=`KPI KPIJustIndMnd`,
    cpi_mom_pct=`KPI Manedsendring`,
    cpi_yoy_pct=`KPI Tolvmanedersendring`,
    cpi_ate_index=`KPI-JAE KPIJustIndMnd`,
    cpi_ate_mom_pct=`KPI-JAE Manedsendring`,
    cpi_ate_yoy_pct=`KPI-JAE Tolvmanedersendring`
  )


# Fetch Unemployment information
ue = get_ssb(
  "13760",
  "valueCodes[Kjonn]=0&valueCodes[Alder]=15-74&valueCodes[Justering]=S&valueCodes[ContentsCode]=ArbledProsArbstyrk&valueCodes[Tid]=from(2006M01)&stub=Tid&heading=Kjonn,Alder,Justering,ContentsCode"
) |> rename(unemployment_rate_sa_pct=2)   


# Euro and dollar exchange rates (NOK per 1 EUR / 1 USD), higher = weaker krone
eur = get_norges_bank("EXR", "M.EUR.NOK.SP") |> rename(eur_nok=value)

usd = get_norges_bank("EXR", "M.USD.NOK.SP") |> rename(usd_nok=value)


# Interest rates (per cent): 3-month interbank rate (NIBOR) and 10-year government bond yield, OECD data via FRED
# (Norges Bank's government rates only start in 2019)
y3m = get_fred("IR3TIB01NOM156N") |> rename(rate_3m=value)

y10y = get_fred("IRLTLT01NOM156N") |> rename(yield_10y=value)



# CPI for electricity (index, 2025=100), table 14700, item 04.5.1
elec = get_ssb(
  "14700",
  "valueCodes[VareTjenesteGrp]=04.5.1&valueCodes[ContentsCode]=KpiIndMnd&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=VareTjenesteGrp,ContentsCode"
) |> rename(cpi_elec=2)

# Household credit, 12-month growth (per cent), table 11599 (credit indicator K2)
credit = get_ssb(
  "11599",
  "valueCodes[Valuta]=00&valueCodes[Lantaker2]=Kred04&valueCodes[ContentsCode]=AarsTrans2&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=Valuta,Lantaker2,ContentsCode"
) |> rename(credit_growth=2)


# Retail sales volume, seasonally adjusted (index, 2021=100), table 07129, retail trade (NACE 47)
retail = get_ssb(
  "07129",
  "valueCodes[NACE]=47&valueCodes[ContentsCode]=VolumSesong&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=NACE,ContentsCode"
) |> rename(retail_volume=2)


# Brent oil price (USD per barrel): daily prices averaged to monthly
brent = get_fred("DCOILBRENTEU") |> 
  mutate(date = floor_date(date, "month")) |> 
  group_by(date) |> 
  summarise(brent_usd = mean(value, na.rm=TRUE))

# US CPI (index, seasonally adjusted)
us_cpi = get_fred("CPIAUCSL") |> rename(us_cpi=value)


# Euro-area inflation (HICP, 12-month rate in per cent), Eurostat dataset prc_hicp_minr
hicp = get_eurostat(
  "prc_hicp_minr",
  filters = list(geo="EA", unit="RCH_A", coicop18="TOTAL"),
  time_format = "date"
) |> 
  select(date=time, hicp_ea=values) |> 
  filter(date >= ym(start_month), date <= ym(end_month)) |> 
  arrange(date)




# Add prev
cpi = cpi |> 
  arrange(date) |>
  mutate(prev_cpi = lag(cpi_index), inflation=100*(log(cpi_index) - log(prev_cpi)))

# Add inflation_12m
cpi = cpi |> mutate(inflation_12m = 100 * (log(cpi_index) - log(lag(cpi_index, 12))))

# Add core_12m
cpi = cpi |> mutate(core_12m = 100 * (log(cpi_ate_index) - log(lag(cpi_ate_index, 12))))

# Add ssb_log
cpi = cpi |> mutate(ssb_log = 100 * log(1 + cpi_yoy_pct/100))

# Add the abs difference between ssb_log and inflation_12m
cpi = cpi |> mutate(gap = abs(ssb_log - inflation_12m))

print("Maximum Gap:")
print(cpi |> summarise(max_gap = max(gap, na.rm = TRUE)))

# Remove the check columns, they are not needed in the dataset
cpi = cpi |> select(-ssb_log, -gap)


# Some series are published one month after CPI, so on the forecast day we only have them up to the previous month.
# known_at_forecast moves their date forward by months_late, so each value is used only when it is really available.
known_at_forecast = function(df, months_late) df |> mutate(date = date %m+% months(months_late))

ue = known_at_forecast(ue, 1)
credit = known_at_forecast(credit, 1)
retail = known_at_forecast(retail, 1)
hicp = known_at_forecast(hicp, 1) 
us_cpi = known_at_forecast(us_cpi, 1)

# Put all series in one named list, so we can check and join them together
series_list = list(
  cpi=cpi, ue=ue, exr=exr, pr=pr,
  eur=eur, usd=usd, y3m=y3m, y10y=y10y,
  elec=elec, credit=credit, retail=retail,
  brent=brent, us_cpi=us_cpi, hicp=hicp
)

# Check first month, last month and number of rows of every series before joining
series_list |> 
  map(\(df) tibble(first=min(df$date), last=max(df$date), rows=nrow(df))) |> 
  list_rbind(names_to="series")

# Merge the datasets through date column
dataset = series_list |> reduce(inner_join, by='date')

# Add term_spread: 10-year yield minus 3-month rate, a falling spread signals that markets expect lower rates and inflation
dataset = dataset |> mutate(term_spread = yield_10y - rate_3m)

# Rename the columns that their names are ambiguous
dataset = dataset |> rename(
  cpi=cpi_index, 
  cpi_mom=cpi_mom_pct, 
  cpi_yoy=cpi_yoy_pct, 
  core=cpi_ate_index, 
  core_mom=cpi_ate_mom_pct,
  core_yoy=cpi_ate_yoy_pct,
  unemp=unemployment_rate_sa_pct,
  krone_i44=i44_index,
  policy_rate=policy_rate_pct
)
glimpse(dataset)

# Check whether any column includes any missing values or not
dataset |> summarise(across(everything(), ~sum(is.na(.x))))

# Check missing months, the sample starts in 2006-02 because unemployment is moved one month later
all_months = tibble(date=seq(ym('2006-02'), ym('2026-08'), by='month'))
all_months |> anti_join(dataset, by='date')


### Exploratory Data Analysis 

summary(dataset)

## Data Quality
# Checking for missing values/observations
colSums(is.na(dataset)) # returns one missing value for us_cpi
dataset |> 
  filter(is.na(us_cpi)) |> 
  select(date, us_cpi) # US cpi missing for 2025-11-01

# Checking for duplicate dates
dataset |> 
  count(date) |> 
  filter(n > 1) # No duplicates

# Checking for missing dates
dataset |> 
  complete(
    date = seq.Date(
      min(date),
      max(date),
      by = "month"
    )
  ) |> 
  filter(!date %in% dataset$date
  ) |> 
  select(date) # No missing dates!

## Descriptive statistics
descriptive_stats <- dataset |> 
  mutate(
    period = if_else(
      date < as.Date("2021-01-01"),
      "Pre-2021",
      "Post-2021"
    )) |>
  group_by(period) |> 
  summarize(
    obs = sum(!is.na(inflation_12m)),
    mean = mean(inflation_12m, na.rm = T),
    median = median(inflation_12m, na.rm = T),
    sd = sd(inflation_12m, na.rm = T),
    min = min(inflation_12m, na.rm = T),
    max = max(inflation_12m, na.rm = T),
    .groups = "drop"
  ) 
descriptive_stats   # Post 2021: 68 obs, Pre 2021: 179 obs


## Times Series Visualization 
ts_12m_inflation <- dataset |> 
  ggplot(aes(date, inflation_12m)
  ) +
  geom_line(color = "grey", linewidth = .8
  ) +
  # Inflation target before 2018 (2.5 %)
  annotate(
    "segment",
    x = min(dataset$date),
    xend = as.Date("2018-03-01"),
    y = 2.5,
    yend = 2.5,
    linetype = "dashed", color = "red", linewidth = 0.3
  ) +
  # Inflation target after 2018 (2 %)
  annotate(
    "segment",
    x = as.Date("2018-03-01"),
    xend = max(dataset$date),
    y = 2,
    yend = 2,
    linetype = "dashed", color = "red", linewidth = 0.3
  ) +
  annotate(
    "segment",
    x = as.Date("2018-03-01"),
    xend = as.Date("2018-03-01"),
    y = 2.5,
    yend = 2,
    linetype = "dashed", color = "red", linewidth = 0.3
  ) +
  theme_minimal() +
  labs(
    title = "12-month Inflation",
    subtitle = "Monthly Observations (2006-2026)",
    x =  "Date",
    y = "Inflation (%)"
  )
ts_12m_inflation

# macroeconomic predictors
predictor_plot <- dataset |> 
  select(date, unemp, cpi, us_cpi,
         policy_rate, yield_10y, credit_growth, 
         krone_i44, eur_nok, usd_nok,
         retail_volume, hicp_ea, brent_usd
  ) |> 
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) |> 
  ggplot(aes(
    date, value, color = variable)
  ) +
  geom_line() +
  facet_wrap(~ variable, 
             scales = "free_y",
             ncol = 3
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  labs(
    title = "Macroeconomic Predictors",
    subtitle = "Not all are included in this graph",
    x = "Year",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11),
    axis.text.x = element_text(size = 8),
    legend.position = "none"
  )
predictor_plot

# Pre- and post-2021 comparison using 12-month inflation
inflation_pre_post_21 <- dataset |> 
  mutate(
    period = if_else(
      date < as.Date("2021-01-01"),
      "Pre-2021",
      "Post-2021"
    ),
    period = factor(period, levels = c("Pre-2021", "Post-2021"))
  ) |> 
  ggplot(aes(date, inflation_12m, color = period)) +
  geom_line() +
  facet_wrap(~ period, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Inflation Before and After 2021",
    x = "Year",
    y = "12-month Inflation (%)"
  ) +
  theme(
    legend.position = "none"
  )
inflation_pre_post_21

# 12-month moving average.
# install.packages("slider")
library(slider)

inflation_12m_ma <- dataset |> 
  arrange(date) |> 
  mutate(cpi_ma12 = slide_dbl(
    cpi_mom,
    .f = mean, 
    .before = 11,
    .complete = T
  )) |> 
  ggplot(aes(x = date)) +
  geom_line(
    aes(y = cpi_mom),
    color = "lightgrey") +
  geom_line(
    aes(y = cpi_ma12),
    color = "darkred") +
  theme_minimal() +
  labs(
    title = "12-month moving average of monthly inflation",
    x = "Year",
    y = "Monthly CPI (%)"
  )
inflation_12m_ma

## ACF and PACF
# install.packages("forecast")
library(forecast)

# ACF
inflation_ts <- dataset |> 
  arrange(date) |> 
  pull(inflation_12m)

acf_12m_inflation <- ggAcf(
  inflation_ts,
  lag.max = 24
) +
  labs(
    title = "Autocorrelation of 12-month Inflation",
    x = "Lag (months)",
    y = "ACF"
  ) +
  theme_minimal()
acf_12m_inflation

# PACF
pacf_12m_inflation <- ggPacf(
  inflation_ts,
  lag.max = 24
) +
  labs(
    title = "Partial Autocorrelation of 12-month Inflation",
    x = "Lag (months)",
    y = "PACF"
  ) + 
  theme_minimal()
pacf_12m_inflation

## ACF and PACF for monthly inflation 
# ACF monthly
inflation_m <- dataset |> 
  arrange(date) |> 
  pull(inflation)

acf_m_inflation <- ggAcf(
  inflation_m,
  lag.max = 24
) +
  labs(
    title = "Autocorrelation of monthly Inflation",
    x = "Lag (months)",
    y = "ACF"
  ) +
  theme_minimal()
acf_m_inflation

# PACF monthly
pacf_m_inflation <- ggPacf(
  inflation_m,
  lag.max = 24
) +
  labs(
    title = "Partial Autocorrelation of monthly Inflation",
    x = "Lag (months)",
    y = "PACF"
  ) +
  theme_minimal()
pacf_m_inflation

# Inspecting seasonality
library(lubridate)

seasonality <- dataset |> 
  mutate(
    month = lubridate::month(date, label = T)
  ) |> 
  group_by(month) |> 
  summarize(
    mean_inflation = mean(inflation, na.rm = T),
    .groups = "drop"
  ) |> 
  ggplot(aes(month, mean_inflation, fill = month)) +
  geom_col() +
  labs(
    title = "Average Monthly Inflation by Calendar Month (seasonality)",
    subtitle = "Norway, 2006-2026",
    x = "Month",
    y = "Average Monthly Inflation (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
seasonality

# Seasonality pre_ and post-2021
seasonality_pre_post_21 <- dataset |>
  mutate(
    month = lubridate::month(date, label = TRUE),
    period = if_else(
      date < as.Date("2021-01-01"),
      "Pre-2021",
      "Post-2021"
    )
  ) |> 
  group_by(period, month) |> 
  summarize(
    avg_inflation = mean(inflation, na.rm = T),
    .groups = "drop"
  ) |> 
  ggplot(aes(month, avg_inflation, fill = period)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Average Monthly Inflation by Calendar Month (seasonality)",
    subtitle = "Comparison of pre- and post-2021",
    x = "Month",
    y = "Average monthly inflation (%)"
  )
seasonality_pre_post_21

## Computing Correlation Matrix
cor_data <- dataset |> 
  arrange(date) |> 
  mutate(eur_nok_growth = 100 * (log(eur_nok) - log(lag(eur_nok))),
         usd_nok_growth = 100 * (log(usd_nok) - log(lag(usd_nok))),
         brent_growth = 100 * (log(brent_usd) - log(lag(brent_usd))),
         krone_i44_growth = 100 * (log(krone_i44) - log(lag(krone_i44))),
         retail_growth = 100 * (log(retail_volume) - log(lag(retail_volume, 12))),
         us_inflation = 100 * (log(us_cpi) - log(lag(us_cpi, 12))),
         electricity_inflation = 100 * (log(cpi_elec) - log(lag(cpi_elec, 12)))
  ) |> 
  select(inflation_12m, unemp, policy_rate, rate_3m, yield_10y, 
         term_spread, credit_growth, eur_nok_growth, usd_nok_growth, 
         brent_growth, krone_i44_growth, retail_growth, us_inflation, 
         electricity_inflation, hicp_ea
  )

cor_matrix <- cor(
  cor_data,
  use = "pairwise.complete.obs"
)

round(cor_matrix, 2)

# Vizualiation of Correlation Matrix
cormatrix <- as.data.frame(as.table(cor_matrix)) |> 
  ggplot(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "darkred",
    midpoint = 0,
    limits = c(-1,1)
  ) +
  coord_equal() +
  labs(
    title = "Correlation Matrix",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
cormatrix

## Computing lagged predictor correlations
horizons <- c(1, 3, 6, 12)

# constructing future inflation targets
forecast_cor_data <- cor_data |> 
  mutate(
    inflation_h1 = lead(inflation_12m, 1),
    inflation_h3 = lead(inflation_12m, 3),
    inflation_h6 = lead(inflation_12m, 6),
    inflation_h12 = lead(inflation_12m, 12)
  )
# identifying predictor columns
predictors <- cor_data |> 
  select(-inflation_12m) |> 
  names()

# Calculating correlations
lagged_correlations <- expand_grid(
  predictor = predictors,
  horizon = horizons
) |> 
  mutate(
    correlation = map2_dbl(
      predictor,
      horizon,
      \(x, h) cor(
        forecast_cor_data[[x]],
        forecast_cor_data[[paste0("inflation_h", h)]],
        use = "complete.obs"
      )
    )
  )

lagged_correlations |> 
  mutate(
    horizon = factor(horizon),
    predictor = factor(predictor)
  ) |> 
  ggplot(aes(
    x = horizon,
    y = predictor,
    fill = correlation
  )) +
  geom_tile() +
  geom_text(
    aes(label = round(correlation, 2))
  ) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "darkred",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Predictor colrrelation with future inflation",
    x = "Forecast Horizons (months)",
    y = "Predictor",
    fill = "Correlation"
  ) +
  theme_minimal()
lagged_correlations

## Stationary test

#install.packages("purrr")
#install.packages("tseries")
library(purrr)
library(tseries)

stationarity_vars <- c(
  "inflation_12m",
  "unemp",
  "policy_rate",
  "credit_growth",
  "eur_nok_growth",
  "usd_nok_growth",
  "brent_growth",
  "krone_i44_growth",
  "retail_growth",
  "us_inflation",
  "electricity_inflation",
  "hicp_ea"
)

adf_results <- map_dfr(stationarity_vars, function(v) {
  
  x <- na.omit(cor_data[[v]])
  
  test <- adf.test(x)
  
  tibble(
    variable = v,
    adf_statistic = unname(test$statistic),
    p_value = test$p.value
  )
})
adf_results



