# Data wrangling and tyding

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
  read_delim(url, delim = ';') |> 
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
  # First we construct the needed url, FRED wants full dates
  url = paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id,
    "&cosd=", start_month, "-01",
    "&coed=", end_month, "-01"
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


# Government yields (per cent): 3-month treasury bills and 10-year bonds
y3m = get_norges_bank("GOVT_GENERIC_RATES", "M.3M.TBIL") |> rename(yield_3m=value)

y10y = get_norges_bank("GOVT_GENERIC_RATES", "M.10Y.GBON") |> rename(yield_10y=value)



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


# Brent oil price (USD per barrel, monthly average) and US CPI (index, seasonally adjusted)
brent = get_fred("POILBREUSDM") |> rename(brent_usd=value)

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

# Rename the columns
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

all_months = tibble(date=seq(ym('2006-01'), ym('2026-08'), by='month'))
all_months |> anti_join(dataset, by='date')