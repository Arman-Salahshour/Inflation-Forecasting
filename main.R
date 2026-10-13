# Data wrangling and tyding

library(tidyverse)

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
) |>
  rename(unemployment_rate_sa_pct=2)   




# A glimpse to the data:
print("CPI:")
glimpse(cpi)
names(cpi)


print("unemployment:")
glimpse(ue)
names(ue)

print("exchange_rate:")
glimpse(exr)
names(exr)


print("policy_rate:")
glimpse(pr)
names(pr)


# Due to using url and apply the following changes earlier, the following lines of code have been commented.
# # Convert the date column type (char) to the (date) type
# 
# date_convertor = function (df) {
#   df |> 
#   mutate(date = ym(date))|> 
#   arrange(date)
# } 
# 
# 
# cpi = date_convertor(cpi)
# ue = date_convertor(ue)
# exr = date_convertor(exr)
# pr = date_convertor(pr)




# Add prev
cpi = cpi |> 
  arrange(date) |>
  mutate(prev_cpi = lag(cpi_index), inflation=100*(log(cpi_index) - log(prev_cpi)))

# Merge the datasets through date column
dataset = cpi |> inner_join(ue, by='date') |> inner_join(exr, by='date') |> inner_join(pr, by='date')
glimpse(dataset)
names(dataset)

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

# Check whether any column includes any missing values or not
dataset |> summarise(across(everything(), ~sum(is.na(.x))))

all_months = tibble(date=seq(ym('2006-01'), ym('2026-08'), by='month'))
all_months |> anti_join(dataset, by='date')






