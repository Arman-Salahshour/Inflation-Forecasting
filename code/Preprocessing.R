# In pdfs/Preprocessing.pdf you can find the notebook of this code, with all of its output
# Run it first, from the project folder, the other two scripts use the datasets it builds

# Data tidying and data wrangling

library(tidyverse)
# install.packages("eurostat")
library(eurostat)


# Download helpers
start_month = "2005-01"
end_month = "2026-08"

# Norges Bank: one series from a dataset and a key
get_norges_bank = function(dataset, key){
  url = paste0(
    "https://data.norges-bank.no/api/data/",
    dataset,
    "/",key,
    "?format=csv&startPeriod=", start_month,
    "&endPeriod=", end_month,
    "&locale=en"
  )
  
  # Keep only the date and the value, and turn the date into a real date
  read_delim(url, delim = ';', show_col_types=FALSE) |>
    select(date=TIME_PERIOD, value=OBS_VALUE) |>
    mutate(date = ym(date))|>
    arrange(date)
}


# SSB: one table, the query picks the series
get_ssb = function(table, query) {
  url = paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/", table,
    "/data?outputFormat=csv&", query
  )
  
  # SSB writes months as 2005M01, we turn them into dates like the other sources
  read_csv(url, show_col_types=FALSE) |>
    mutate(date=ym(str_replace(Tid, "M", "-")), .before=1) |>
    select(-Tid) |>
    filter(date <= ym(end_month))
}

# FRED: one series by its id
get_fred = function(series_id){
  # FRED wants full dates, so we use the first day of start_month and the last day of end_month
  url = paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id,
    "&cosd=", start_month, "-01",
    "&coed=", ym(end_month) %m+% months(1) - days(1)
  )
  
  # FRED writes missing values as ".", and its column names change, so we rename by position
  read_csv(url, na = c("", "."), show_col_types=FALSE) |>
    rename(date=1, value=2) |>
    mutate(date = as_date(date)) |>
    arrange(date)
}


# Import-weighted krone index (I44), higher means a weaker krone
exr = get_norges_bank("EXR", "M.I44.NOK.SP") |> rename(i44_index=value)

# Norges Bank policy rate
pr = get_norges_bank("IR", "M.KPRA.SD.R") |> rename(policy_rate_pct=value)


# Headline CPI and core CPI (CPI-ATE): index, monthly change and 12-month change
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


# Unemployment rate, ages 15-74, seasonally adjusted (table 13760)
ue = get_ssb(
  "13760",
  "valueCodes[Kjonn]=0&valueCodes[Alder]=15-74&valueCodes[Justering]=S&valueCodes[ContentsCode]=ArbledProsArbstyrk&valueCodes[Tid]=from(2006M01)&stub=Tid&heading=Kjonn,Alder,Justering,ContentsCode"
) |> rename(unemployment_rate_sa_pct=2)


# NOK per euro and per dollar, higher means a weaker krone
eur = get_norges_bank("EXR", "M.EUR.NOK.SP") |> rename(eur_nok=value)

usd = get_norges_bank("EXR", "M.USD.NOK.SP") |> rename(usd_nok=value)


# 3-month interbank rate and 10-year government bond yield, OECD data from FRED
# We use FRED because Norges Bank's government rates only start in 2019
y3m = get_fred("IR3TIB01NOM156N") |> rename(rate_3m=value)

y10y = get_fred("IRLTLT01NOM156N") |> rename(yield_10y=value)



# CPI for electricity (table 14700, item 04.5.1)
elec = get_ssb(
  "14700",
  "valueCodes[VareTjenesteGrp]=04.5.1&valueCodes[ContentsCode]=KpiIndMnd&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=VareTjenesteGrp,ContentsCode"
) |> rename(cpi_elec=2)

# Household credit, 12-month growth in per cent (table 11599)
credit = get_ssb(
  "11599",
  "valueCodes[Valuta]=00&valueCodes[Lantaker2]=Kred04&valueCodes[ContentsCode]=AarsTrans2&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=Valuta,Lantaker2,ContentsCode"
) |> rename(credit_growth=2)


# Retail sales volume, seasonally adjusted (table 07129, retail trade)
retail = get_ssb(
  "07129",
  "valueCodes[NACE]=47&valueCodes[ContentsCode]=VolumSesong&valueCodes[Tid]=from(2005M01)&stub=Tid&heading=NACE,ContentsCode"
) |> rename(retail_volume=2)


# Brent oil price in USD, daily prices averaged to months
brent = get_fred("DCOILBRENTEU") |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(brent_usd = mean(value, na.rm=TRUE))

# US CPI, seasonally adjusted
us_cpi = get_fred("CPIAUCSL") |> rename(us_cpi=value)


# Euro area inflation, 12-month rate in per cent (Eurostat)
hicp = get_eurostat(
  "prc_hicp_minr",
  filters = list(geo="EA", unit="RCH_A", coicop18="TOTAL"),
  time_format = "date"
) |>
  select(date=time, hicp_ea=values) |>
  filter(date >= ym(start_month), date <= ym(end_month)) |>
  arrange(date)


# Monthly inflation as a log change from the previous month
cpi = cpi |>
  arrange(date) |>
  mutate(prev_cpi = lag(cpi_index), inflation=100*(log(cpi_index) - log(prev_cpi)))

# 12-month inflation, our target
cpi = cpi |> mutate(inflation_12m = 100 * (log(cpi_index) - log(lag(cpi_index, 12))))

# 12-month core inflation, for the robustness check
cpi = cpi |> mutate(core_12m = 100 * (log(cpi_ate_index) - log(lag(cpi_ate_index, 12))))

# Check our numbers against SSB's: put SSB's 12-month rate in log form and compare
cpi = cpi |> mutate(ssb_log = 100 * log(1 + cpi_yoy_pct/100))

cpi = cpi |> mutate(gap = abs(ssb_log - inflation_12m))

print("Maximum Gap:")
print(cpi |> summarise(max_gap = max(gap, na.rm = TRUE)))

# The check columns are not needed any more
cpi = cpi |> select(-ssb_log, -gap)


# Some series come out one month after CPI, so on the forecast date we only have last month's value
# known_at_forecast moves their dates one month later, so each value is used only once it is published
known_at_forecast = function(df, months_late) df |> mutate(date = date %m+% months(months_late))

ue = known_at_forecast(ue, 1)
credit = known_at_forecast(credit, 1)
retail = known_at_forecast(retail, 1)
hicp = known_at_forecast(hicp, 1)
us_cpi = known_at_forecast(us_cpi, 1)

# All series in one named list, to check and join them together
series_list = list(
  cpi=cpi, ue=ue, exr=exr, pr=pr,
  eur=eur, usd=usd, y3m=y3m, y10y=y10y,
  elec=elec, credit=credit, retail=retail,
  brent=brent, us_cpi=us_cpi, hicp=hicp
)

# 12-month log change for prices and indexes, 12-month difference for rates
dl12 = function(x) 100 * (log(x) - log(lag(x, 12)))
d12 = function(x) x - lag(x, 12)


# First month, last month and number of rows of each series before the join
series_list |>
  map(\(df) tibble(first=min(df$date), last=max(df$date), rows=nrow(df))) |>
  list_rbind(names_to="series")

# Join everything by date, only months that all series have are kept
dataset = series_list |> reduce(inner_join, by='date')

# Term spread: long rate minus short rate, a falling spread often means markets expect lower rates and inflation
dataset = dataset |> mutate(term_spread = yield_10y - rate_3m)

# Shorter and clearer column names
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

# US CPI is missing for October 2025 (US government shutdown), we fill it with the midpoint of its neighbours
dataset = dataset |>
  arrange(date) |>
  mutate(us_cpi = if_else(is.na(us_cpi), exp((log(lag(us_cpi)) + log(lead(us_cpi))) / 2), us_cpi))

# Make the predictors stationary: 12-month log change for prices, 12-month difference for rates
prices = c("krone_i44", "eur_nok", "usd_nok", "cpi_elec", "us_cpi", "retail_volume", "brent_usd")
rates = c("unemp", "policy_rate", "rate_3m", "yield_10y", "credit_growth")

dataset = dataset |>
  mutate(across(all_of(prices), dl12, .names="dl12_{.col}"))

dataset = dataset |>
  mutate(across(all_of(rates), d12, .names="d12_{.col}"))

# Inflation today and in each of the last 12 months, all known on the forecast date
# Done before the filter, so the 2007 rows still see their 2006 history
lags = map(0:12, \(k) lag(dataset$inflation_12m, k)) |>
  set_names(paste0('inflation_12m_lag', 0:12)) |>
  as_tibble()

# Targets: inflation 1, 3, 6 and 12 months ahead
# The last h rows are NA because that future has not happened yet
leads = map(c(1, 3, 6, 12), \(k) lead(dataset$inflation_12m, k)) |>
  set_names(paste0('inflation_12m_lead', c(1, 3, 6, 12))) |>
  as_tibble()

# Table for the models: dataset plus the lags and the targets
# dataset itself stays as it is for the EDA
ml_dataset = bind_cols(dataset, lags)
ml_dataset = bind_cols(ml_dataset, leads)


# Drop the first 12 months, they have no value a year earlier for the 12-month changes and lag 12
dataset = dataset |> filter(date >= ym("2007-02"))
ml_dataset = ml_dataset |> filter(date >= ym("2007-02"))
glimpse(dataset)
glimpse(ml_dataset)

# Count missing values in each column
dataset |> summarise(across(everything(), ~sum(is.na(.x))))
ml_dataset |> summarise(across(everything(), ~sum(is.na(.x))))


# Check that no month is missing between 2007-02 and 2026-08
# The sample starts in 2007-02 because unemployment starts in 2006-01, is moved one month later,
# and the 12-month changes need one year of history
all_months = tibble(date=seq(ym('2007-02'), ym('2026-08'), by='month'))
all_months |> anti_join(dataset, by='date')


