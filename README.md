# Persistence Is Hard to Beat: Forecasting Norwegian Inflation with Machine Learning

Term paper in ECON3170/4170, autumn 2026, by Arman Salahshour and Truls Holmdal.

We ask whether machine learning methods (LASSO, ridge, elastic net and a random forest) forecast Norwegian 12-month inflation better than simple benchmarks (a random walk, an autoregressive model and a small OLS model). The forecasts are made 1, 3, 6 and 12 months ahead, every month from January 2015 to August 2026.

## Folders

| Folder | What is in it |
|---|---|
| `code/` | The three R scripts |
| `notebooks/` | A notebook version of each script (`.qmd`), with the rendered output (`.html`) |
| `pdfs/` | The rendered notebooks as PDF files, with every figure and table |
| `report/` | The term paper (`term-paper.qmd`) and its references (`references.bib`) |
| `data/` | The dataset and the modelling table saved by the preprocessing notebook |
| `images/` | The figures saved by the code |
| `results/` | The forecasts, evaluation tables and LASSO coefficients saved by the code |

## The code

| Script | What it does | Section of the paper |
|---|---|---|
| `code/Preprocessing.R` | Downloads all series from the APIs, applies publication lags and transformations, builds the dataset and the modelling table | Data and preparation |
| `code/EDA.R` | Descriptive figures, stationarity tests, correlations, structural breaks and rolling correlations | Data and preparation |
| `code/Modelling.R` | Expanding-window forecasts for all models, evaluation tables, Diebold-Mariano tests, the LASSO selection analysis and the robustness checks with core inflation and a rolling window | Methods, Results and evaluation |

## How to run

1. Open `Project.Rproj` in RStudio, so the working directory is the project folder.
2. Install the packages if needed: `tidyverse`, `eurostat`, `tseries`, `slider`, `strucchange`, `glmnet`, `ranger`, `forecast`.
3. Run `code/Preprocessing.R`, then `code/EDA.R`, then `code/Modelling.R`, in the same R session. The forecasting loops take a few minutes.
4. Render `report/term-paper.qmd`. It only reads the saved files in `results/` and `images/`.

The notebooks can also be run on their own, in the same order. `Preprocessing.qmd` saves the data, and the other two load it. Keep `_quarto.yml` in the project folder: it lets the report use the figures in `images/`.

## Data sources

Statistics Norway (SSB), Norges Bank, FRED and Eurostat. All series are downloaded from the providers' public APIs, so no data file needs to be added by hand.
