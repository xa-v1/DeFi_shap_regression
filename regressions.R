library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

# ----------
# UNI - BULL
# ----------
uni_reg_bull <- lm(UNI_return ~ delta_UNI_SPOT_VOLUME + delta_UNI_DAU + delta_UNI_CAPITAL_EFFICIENCY +
                     delta_UNI_TVL + crypto_mkt_beta_pca, data = dex_bull)
uni_robust_bull <- coeftest(uni_reg_bull, vcov = vcovHC(uni_reg_bull, type = "HC3"))
uni_robust_bull

# ----------
# UNI - BEAR
# ----------
uni_reg_bear <- lm(UNI_return ~ delta_UNI_SPOT_VOLUME + delta_UNI_DAU + delta_UNI_CAPITAL_EFFICIENCY +
                     delta_UNI_TVL + crypto_mkt_beta_pca, data = dex_bear)
uni_robust_bear <- coeftest(uni_reg_bear, vcov = vcovHC(uni_reg_bear, type = "HC3"))
uni_robust_bear

# ----------
# AERO - BULL
# ----------
aero_reg_bull <- lm(AERO_return ~ delta_AERO_SPOT_VOLUME + delta_AERO_DAU + delta_AERO_CAPITAL_EFFICIENCY +
                      delta_AERO_TVL + crypto_mkt_beta_pca, data = dex_bull)
aero_robust_bull <- coeftest(aero_reg_bull, vcov = vcovHC(aero_reg_bull, type = "HC3"))
aero_robust_bull
# ----------
# AERO - BEAR
# ----------
aero_reg_bear <- lm(AERO_return ~ delta_AERO_SPOT_VOLUME + delta_AERO_DAU + delta_AERO_CAPITAL_EFFICIENCY +
                      delta_AERO_TVL + crypto_mkt_beta_pca, data = dex_bear)
aero_robust_bear <- coeftest(aero_reg_bear, vcov = vcovHC(aero_reg_bear, type = "HC3"))
aero_robust_bear
# ----------
# CAKE - BULL
# ----------
cake_reg_bull <- lm(CAKE_return ~ delta_CAKE_SPOT_VOLUME + delta_CAKE_DAU + delta_CAKE_CAPITAL_EFFICIENCY +
                      delta_CAKE_TVL + crypto_mkt_beta_pca, data = dex_bull)
cake_robust_bull <- coeftest(cake_reg_bull, vcov = vcovHC(cake_reg_bull, type = "HC3"))
cake_robust_bull
# ----------
# CAKE - BEAR
# ----------
cake_reg_bear <- lm(CAKE_return ~ delta_CAKE_SPOT_VOLUME + delta_CAKE_DAU + delta_CAKE_CAPITAL_EFFICIENCY +
                      delta_CAKE_TVL + crypto_mkt_beta_pca, data = dex_bear)
cake_robust_bear <- coeftest(cake_reg_bear, vcov = vcovHC(cake_reg_bear, type = "HC3"))
cake_robust_bear
# ----------
# AAVE - BULL
# ----------
aave_reg_bull <- lm(AAVE_return ~ delta_AAVE_LENDING_LOANS + delta_AAVE_LENDING_DEPOSITS + delta_AAVE_FEES +
                      delta_AAVE_TVL + crypto_mkt_beta_pca, data = lending_bull)
aave_robust_bull <- coeftest(aave_reg_bull, vcov = vcovHC(aave_reg_bull, type = "HC3"))
aave_robust_bull
# ----------
# AAVE - BEAR
# ----------
aave_reg_bear <- lm(AAVE_return ~ delta_AAVE_LENDING_LOANS + delta_AAVE_LENDING_DEPOSITS + delta_AAVE_FEES +
                      delta_AAVE_TVL + crypto_mkt_beta_pca, data = lending_bear)
aave_robust_bear <- coeftest(aave_reg_bear, vcov = vcovHC(aave_reg_bear, type = "HC3"))
aave_robust_bear
# ----------
# MORPHO - BULL
# ----------
morpho_reg_bull <- lm(MORPHO_return ~ delta_MORPHO_LENDING_LOANS + delta_MORPHO_LENDING_DEPOSITS + delta_MORPHO_FEES +
                        delta_MORPHO_TVL + crypto_mkt_beta_pca, data = lending_bull)
morpho_robust_bull <- coeftest(morpho_reg_bull, vcov = vcovHC(morpho_reg_bull, type = "HC3"))
morpho_robust_bull
# ----------
# MORPHO - BEAR
# ----------
morpho_reg_bear <- lm(MORPHO_return ~ delta_MORPHO_LENDING_LOANS + delta_MORPHO_LENDING_DEPOSITS + delta_MORPHO_FEES +
                        delta_MORPHO_TVL + crypto_mkt_beta_pca, data = lending_bear)
morpho_robust_bear <- coeftest(morpho_reg_bear, vcov = vcovHC(morpho_reg_bear, type = "HC3"))
morpho_robust_bear
# ----------
# HYPE - BULL
# ----------
hype_reg_bull <- lm(HYPE_return ~ delta_HYPE_PERP_VOLUME + delta_HYPE_PERP_DAU + delta_HYPE_REVENUE +
                      delta_HYPE_CHAIN_TVL + crypto_mkt_beta_pca, data = perp_bull)
hype_robust_bull <- coeftest(hype_reg_bull, vcov = vcovHC(hype_reg_bull, type = "HC3"))
hype_robust_bull
# ----------
# HYPE - BEAR
# ----------
hype_reg_bear <- lm(HYPE_return ~ delta_HYPE_PERP_VOLUME + delta_HYPE_PERP_DAU + delta_HYPE_REVENUE +
                      delta_HYPE_CHAIN_TVL + crypto_mkt_beta_pca, data = perp_bear)
hype_robust_bear <- coeftest(hype_reg_bear, vcov = vcovHC(hype_reg_bear, type = "HC3"))
hype_robust_bear
# ----------
# ETHFI - BULL
# ----------
ethfi_reg_bull <- lm(ETHFI_return ~ delta_ETHFI_CARD_VOLUME + delta_ETHFI_NUM_CARDS + delta_ETHFI_FEES +
                       delta_ETHFI_TVL + crypto_mkt_beta_pca, data = ethfi_bull)
ethfi_robust_bull <- coeftest(ethfi_reg_bull, vcov = vcovHC(ethfi_reg_bull, type = "HC3"))
ethfi_robust_bull
# ----------
# ETHFI - BEAR
# ----------
ethfi_reg_bear <- lm(ETHFI_return ~ delta_ETHFI_CARD_VOLUME + delta_ETHFI_NUM_CARDS + delta_ETHFI_FEES +
                       delta_ETHFI_TVL + crypto_mkt_beta_pca, data = ethfi_bear)
ethfi_robust_bear <- coeftest(ethfi_reg_bear, vcov = vcovHC(ethfi_reg_bear, type = "HC3"))
ethfi_robust_bear
# ----------
# PUMP - BULL
# ----------
pump_reg_bull <- lm(PUMP_return ~ delta_PUMP_TOKENS_GRADUATED + delta_PUMP_BUYBACKS_NATIVE + delta_PUMP_REVENUE +
                      delta_PUMP_LAUNCHPAD_VOLUMES + crypto_mkt_beta_pca, data = pump_bull)
pump_robust_bull <- coeftest(pump_reg_bull, vcov = vcovHC(pump_reg_bull, type = "HC3"))
pump_robust_bull
# ----------
# PUMP - BEAR
# ----------
pump_reg_bear <- lm(PUMP_return ~ delta_PUMP_TOKENS_GRADUATED + delta_PUMP_BUYBACKS_NATIVE + delta_PUMP_REVENUE +
                      delta_PUMP_LAUNCHPAD_VOLUMES + crypto_mkt_beta_pca, data = pump_bear)
pump_robust_bear <- coeftest(pump_reg_bear, vcov = vcovHC(pump_reg_bear, type = "HC3"))
pump_robust_bear