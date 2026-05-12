# Beyond Beta: Decomposing the Fundamental Drivers of DeFi Token Returns Across Market Regimes

**Oregon Blockchain Group** | Xavier Vortigern

---

## Overview

DeFi token prices are widely observed to move in lockstep with benchmark tokens like Bitcoin and Ethereum, but does that mean protocol fundamentals don't matter? This project investigates that question by decomposing DeFi token returns into two components — systematic market exposure (market beta) and protocol-specific fundamentals — and measuring how the relative contribution of each shifts across bull and bear market regimes.

Rather than stopping at correlation, this study uses **Shapley value regression** to attribute token returns across four DeFi sectors: decentralized exchanges (DEX), lending, perpetuals, and a miscellaneous category. Market regimes are defined using a Bitcoin 200-day moving average filter.

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
regressions.R      # OLS regressions for each token × regime
shapley_decomp.R   # Shapley value decomposition and heatmap visualization
robustness.R       # Robustness checks
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
3. Run scripts in order: `data_clean.R` → `regressions.R` → `shapley_decomp.R`

---

*This project was produced as part of research conducted at the Oregon Blockchain Group, University of Oregon.*
