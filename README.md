# Beyond Beta: Decomposing the Fundamental Drivers of DeFi Token Returns Across Market Regimes

**Oregon Blockchain Group** | Xavier Vortigern

---

## Overview

DeFi token prices are widely observed to move in lockstep with benchmark tokens like Bitcoin and Ethereum, but does that mean protocol fundamentals don't matter? This project investigates that question by decomposing DeFi token returns into two components: systematic market exposure (market beta) and protocol-specific fundamentals. Additionally, it measures how the relative contribution of each shifts across bull and bear market regimes.

This study uses **Shapley value regression** to attribute token returns across four DeFi sectors: decentralized exchanges (DEX), lending, perpetuals, and a miscellaneous category. Market regimes are defined using a Bitcoin 200-day moving average filter.

**Tokens analyzed:** UNI, CAKE, AERO, AAVE, MORPHO, HYPE, ETHFI, PUMP

---

## Key Findings

- **Market beta dominates most tokens**, but the degree varies significantly by sector and regime.
- **Lending protocols (AAVE, MORPHO) are the clearest exception** — fundamental metrics like lending deposits and TVL account for the majority of return attribution in both regimes, with market beta barely registering.
- **Hyperliquid (HYPE) shows an unusual regime reversal** — market beta is higher in bull markets (59.5%) than in bear markets (39.8%), where fundamentals collectively account for over 60% of attribution. This is the opposite of the pattern seen in most other tokens.
- **DEX tokens generally show stronger fundamental contribution in bear markets** — UNI's market beta drops from 57.9% (bull) to 39.4% (bear), with spot volume and capital efficiency picking up the difference.
- **PUMP and ETHFI are heavily market beta-driven** in bear markets (85.2% and 77.6% respectively), suggesting structurally distinct protocols are not immune to systematic risk during downturns.

---

## Methodology

- **Market beta:** Constructed as the first principal component (PCA) of daily log returns for BTC, ETH, and SOL
- **Fundamentals:** Sector-specific on-chain metrics sourced from Artemis, transformed to log or percentage changes
- **Regime classification:** Bull/bear defined by whether BTC price is above or below its 200-day moving average
- **Attribution:** Shapley value regression via the `kernelshap` and `shapviz` packages in R

---

## Data Sources

- On-chain fundamentals: [Artemis](https://www.artemis.xyz/)
- Price data: Yahoo Finance via `tidyquant`
- Sample period: December 1, 2024 – present (PUMP begins July 22, 2025)

---

## Repository Structure

```
data_clean.R       # Data ingestion, cleaning, PCA market beta construction, regime classification
robustness.R       # Robustness checks
regressions.R      # OLS regressions for each token and regime
shapley_decomp.R   # Shapley value decomposition and heatmap visualization
data/              # CSV files for all on-chain and price data
```

---

## Requirements

```r
install.packages(c(
  "tidyverse", "tidyquant", "lubridate", "dplyr",
  "kernelshap", "shapviz", "ggplot2", "writexl", "tidyr"
))
```

---

## Usage

1. Clone the repository
2. Place the CSV files from the `data/` folder in your working directory
3. Run scripts in order: `data_clean.R` → `robustness.r` → `regressions.R` → `shapley_decomp.R`

## Replicating This Analysis for Your Own Tokens

This section explains how to adapt the pipeline to analyze tokens and on-chain metrics of your own choosing.

### Step 1: Pull Your Data with Artemis Sheets

On-chain fundamentals are sourced using the [Artemis Sheets](https://www.artemis.xyz/) Google Sheets extension. For each sector you want to analyze, create a Google Sheet and use the Artemis extension to pull daily metrics for your token(s).

The column structure expected by `data_clean.R` is:

| Column | Content |
|--------|---------|
| date | `MM/DD/YYYY` format |
| price | Daily close price (USD) |
| metric_1 | Demand metric (e.g. volume) |
| metric_2 | Engagement metric (e.g. DAU, deposits) |
| metric_3 | Fee or revenue metric |
| metric_4 | TVL |

Multiple tokens can share a single sheet by placing each token's block in adjacent column groups separated by a blank column.

### Step 2: Connect Your Sheet to `data_clean.R`

The script reads data using `read_sheet()` from the `googlesheets4` package. Authenticate once with `gs4_auth()`, then point each call to your own sheet URL:

```r
gs4_auth()

your_metrics     <- read_sheet("YOUR_SHEET_URL")
```

Then source it at the top of `data_clean.R`:

```r
source("sheets_config.R")
dex_metrics <- read_sheet(DEX_SHEET)
```

### Step 3: Update Column Names and Token Tickers

Each sector block in `data_clean.R` contains a `rename()` call that maps column positions to names following the pattern `TICKER_METRICNAME`. Replace all instances of the original tickers (e.g., `UNI`, `CAKE`, `AAVE`) with your own. These names propagate into `regressions.R` and `shapley_decomp.R`, so consistency across all three files is required.

Columns are referenced by **position** in the initial `rename()` call. If your sheet has a different column count or ordering, update the position numbers before running.

### Step 4: Compute Log-Difference Returns

The regression uses log-differenced fundamentals as regressors. After renaming, add a `mutate()` block for each metric:

```r
delta_TOKEN_METRIC = log(TOKEN_METRIC / dplyr::lag(TOKEN_METRIC))
```

If a metric can take zero values (e.g., buybacks, token launches), use the `+1` offset used for PUMP to avoid undefined log values:

```r
delta_TOKEN_METRIC = log(TOKEN_METRIC + 1) / (dplyr::lag(TOKEN_METRIC) + 1)
```

### Step 5: Update `regressions.R` and `shapley_decomp.R`

Both scripts are structured as token-by-token blocks. Add or replace blocks using the same pattern, with your token's return as the dependent variable and your log-differenced fundamentals alongside `crypto_mkt_beta_pca` as regressors:

```r
model_TOKEN_bull <- lm(
  TOKEN_return ~ crypto_mkt_beta_pca + delta_TOKEN_METRIC1 + delta_TOKEN_METRIC2 + ...,
  data = token_bull
)
```

The market beta factor (PC1 of BTC/ETH/SOL log returns) and bull/bear regime classification (BTC 200-day moving average) are constructed automatically from Yahoo Finance via `tidyquant` and require no modification.

### Notes

- A minimum of roughly 60–90 observations per regime is advisable for stable Shapley decompositions. Tokens with short histories or sparse regime transitions may produce unreliable attribution estimates.
- The script drops all rows with any `NA` via `drop_na()` at the end of each sector block. Missing days in your Artemis data will silently reduce your effective sample — check coverage before running.
- `kernelshap` run time scales with the number of regressors. More than 6–7 features per model will noticeably increase computation time.
