# Norwegian macro data project

This project collects and combines monthly macroeconomic time series for Norway and related international indicators in R. The analysis uses public data from Norges Bank, Statistics Norway (SSB), Eurostat, and FRED, then joins the series by date to create one monthly dataset for macro analysis.

## Included data

The script downloads and combines series such as:

- CPI and core inflation
- Policy rate and yield curves
- Exchange rates
- Unemployment
- Credit growth
- Retail sales
- Electricity CPI
- Brent oil price
- Euro-area inflation
- US CPI

## Files

- `main.R` – downloads the series, cleans the data, joins the monthly variables, and checks coverage.

## How to run

1. Open the project in RStudio using `Project.Rproj`.
2. Install the required packages if needed:
   - `tidyverse`
   - `eurostat`
   - `slider`
   - `forecast`
   - `tseries`
3. Run `main.R`.

The script will fetch the latest available data from the source APIs and construct a merged monthly dataset for analysis.
