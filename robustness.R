library(tseries)
# --------------------
# Confirm Stationary
# --------------------

# AAVE
adf.test(na.omit(lending_metrics$delta_AAVE_LENDING_LOANS))
adf.test(na.omit(lending_metrics$delta_AAVE_LENDING_DEPOSITS))
adf.test(na.omit(lending_metrics$delta_AAVE_FEES))
adf.test(na.omit(lending_metrics$delta_AAVE_TVL))
adf.test(na.omit(lending_metrics$AAVE_return))

# MORPHO
adf.test(na.omit(lending_metrics$delta_MORPHO_LENDING_LOANS))
adf.test(na.omit(lending_metrics$delta_MORPHO_LENDING_DEPOSITS))
adf.test(na.omit(lending_metrics$delta_MORPHO_FEES))
adf.test(na.omit(lending_metrics$delta_MORPHO_TVL))
adf.test(na.omit(lending_metrics$MORPHO_return))

# UNI
adf.test(na.omit(dex_metrics$delta_UNI_SPOT_VOLUME))
adf.test(na.omit(dex_metrics$delta_UNI_DAU))
adf.test(na.omit(dex_metrics$delta_UNI_CAPITAL_EFFICIENCY))
adf.test(na.omit(dex_metrics$delta_UNI_TVL))
adf.test(na.omit(dex_metrics$UNI_return))

# CAKE
adf.test(na.omit(dex_metrics$delta_CAKE_SPOT_VOLUME))
adf.test(na.omit(dex_metrics$delta_CAKE_DAU))
adf.test(na.omit(dex_metrics$delta_CAKE_CAPITAL_EFFICIENCY))
adf.test(na.omit(dex_metrics$delta_CAKE_TVL))
adf.test(na.omit(dex_metrics$CAKE_return))

# AERO
adf.test(na.omit(dex_metrics$delta_AERO_SPOT_VOLUME))
adf.test(na.omit(dex_metrics$delta_AERO_DAU))
adf.test(na.omit(dex_metrics$delta_AERO_CAPITAL_EFFICIENCY))
adf.test(na.omit(dex_metrics$delta_AERO_TVL))
adf.test(na.omit(dex_metrics$AERO_return))

# ETHFI
adf.test(na.omit(ethfi_metrics$delta_ETHFI_CARD_VOLUME))
adf.test(na.omit(ethfi_metrics$delta_ETHFI_NUM_CARDS))
adf.test(na.omit(ethfi_metrics$delta_ETHFI_FEES))
adf.test(na.omit(ethfi_metrics$delta_ETHFI_TVL))
adf.test(na.omit(ethfi_metrics$ETHFI_return))

# PUMP
adf.test(na.omit(pump_metrics$delta_PUMP_TOKENS_GRADUATED))
adf.test(na.omit(pump_metrics$delta_PUMP_BUYBACKS_NATIVE))
adf.test(na.omit(pump_metrics$delta_PUMP_REVENUE))
adf.test(na.omit(pump_metrics$delta_PUMP_LAUNCHPAD_VOLUMES))
adf.test(na.omit(pump_metrics$PUMP_return))

# HYPE
adf.test(na.omit(perp_metrics$delta_HYPE_PERP_VOLUME))
adf.test(na.omit(perp_metrics$delta_HYPE_PERP_DAU))
adf.test(na.omit(perp_metrics$delta_HYPE_REVENUE))
adf.test(na.omit(perp_metrics$delta_HYPE_CHAIN_TVL))
adf.test(na.omit(perp_metrics$HYPE_return))

# --------------------
# Stationarity Summary
# --------------------

adf_pvals <- c(
  AAVE_LENDING_LOANS              = adf.test(na.omit(lending_metrics$delta_AAVE_LENDING_LOANS))$p.value,
  AAVE_LENDING_DEPOSITS           = adf.test(na.omit(lending_metrics$delta_AAVE_LENDING_DEPOSITS))$p.value,
  AAVE_FEES                       = adf.test(na.omit(lending_metrics$delta_AAVE_FEES))$p.value,
  AAVE_TVL                        = adf.test(na.omit(lending_metrics$delta_AAVE_TVL))$p.value,
  AAVE_return                     = adf.test(na.omit(lending_metrics$AAVE_return))$p.value,
  MORPHO_LENDING_LOANS            = adf.test(na.omit(lending_metrics$delta_MORPHO_LENDING_LOANS))$p.value,
  MORPHO_LENDING_DEPOSITS         = adf.test(na.omit(lending_metrics$delta_MORPHO_LENDING_DEPOSITS))$p.value,
  MORPHO_FEES                     = adf.test(na.omit(lending_metrics$delta_MORPHO_FEES))$p.value,
  MORPHO_TVL                      = adf.test(na.omit(lending_metrics$delta_MORPHO_TVL))$p.value,
  MORPHO_return                   = adf.test(na.omit(lending_metrics$MORPHO_return))$p.value,
  UNI_SPOT_VOLUME                 = adf.test(na.omit(dex_metrics$delta_UNI_SPOT_VOLUME))$p.value,
  UNI_DAU                         = adf.test(na.omit(dex_metrics$delta_UNI_DAU))$p.value,
  UNI_CAPITAL_EFFICIENCY          = adf.test(na.omit(dex_metrics$delta_UNI_CAPITAL_EFFICIENCY))$p.value,
  UNI_TVL                         = adf.test(na.omit(dex_metrics$delta_UNI_TVL))$p.value,
  UNI_return                      = adf.test(na.omit(dex_metrics$UNI_return))$p.value,
  CAKE_SPOT_VOLUME                = adf.test(na.omit(dex_metrics$delta_CAKE_SPOT_VOLUME))$p.value,
  CAKE_DAU                        = adf.test(na.omit(dex_metrics$delta_CAKE_DAU))$p.value,
  CAKE_CAPITAL_EFFICIENCY         = adf.test(na.omit(dex_metrics$delta_CAKE_CAPITAL_EFFICIENCY))$p.value,
  CAKE_TVL                        = adf.test(na.omit(dex_metrics$delta_CAKE_TVL))$p.value,
  CAKE_return                     = adf.test(na.omit(dex_metrics$CAKE_return))$p.value,
  AERO_SPOT_VOLUME                = adf.test(na.omit(dex_metrics$delta_AERO_SPOT_VOLUME))$p.value,
  AERO_DAU                        = adf.test(na.omit(dex_metrics$delta_AERO_DAU))$p.value,
  AERO_CAPITAL_EFFICIENCY         = adf.test(na.omit(dex_metrics$delta_AERO_CAPITAL_EFFICIENCY))$p.value,
  AERO_TVL                        = adf.test(na.omit(dex_metrics$delta_AERO_TVL))$p.value,
  AERO_return                     = adf.test(na.omit(dex_metrics$AERO_return))$p.value,
  ETHFI_CARD_VOLUME               = adf.test(na.omit(ethfi_metrics$delta_ETHFI_CARD_VOLUME))$p.value,
  ETHFI_NUM_CARDS                 = adf.test(na.omit(ethfi_metrics$delta_ETHFI_NUM_CARDS))$p.value,
  ETHFI_FEES                      = adf.test(na.omit(ethfi_metrics$delta_ETHFI_FEES))$p.value,
  ETHFI_TVL                       = adf.test(na.omit(ethfi_metrics$delta_ETHFI_TVL))$p.value,
  ETHFI_return                    = adf.test(na.omit(ethfi_metrics$ETHFI_return))$p.value,
  PUMP_TOKENS_GRADUATED           = adf.test(na.omit(pump_metrics$delta_PUMP_TOKENS_GRADUATED))$p.value,
  PUMP_BUYBACKS_NATIVE            = adf.test(na.omit(pump_metrics$delta_PUMP_BUYBACKS_NATIVE))$p.value,
  PUMP_REVENUE                    = adf.test(na.omit(pump_metrics$delta_PUMP_REVENUE))$p.value,
  PUMP_LAUNCHPAD_VOLUMES          = adf.test(na.omit(pump_metrics$delta_PUMP_LAUNCHPAD_VOLUMES))$p.value,
  PUMP_return                     = adf.test(na.omit(pump_metrics$PUMP_return))$p.value,
  HYPE_PERP_VOLUME = adf.test(na.omit(perp_metrics$delta_HYPE_PERP_VOLUME))$p.value,
  HYPE_PERP_DAU                   = adf.test(na.omit(perp_metrics$delta_HYPE_PERP_DAU))$p.value,
  HYPE_REVENUE                    = adf.test(na.omit(perp_metrics$delta_HYPE_REVENUE))$p.value,
  HYPE_CHAIN_TVL                  = adf.test(na.omit(perp_metrics$delta_HYPE_CHAIN_TVL))$p.value,
  HYPE_return                     = adf.test(na.omit(perp_metrics$HYPE_return))$p.value
)

if (all(adf_pvals < 0.05)) {
  message("All series pass ADF test for stationarity (non-stationarity rejected for all)")
} else {
  failed <- names(adf_pvals)[adf_pvals >= 0.05]
  message("WARNING: The following series fail ADF test for stationarity: ", paste(failed, collapse = ", "))
}