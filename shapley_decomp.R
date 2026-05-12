library(kernelshap)
library(shapviz)
library(ggplot2)
library(writexl)
library(tidyr)
library(dplyr)

# ----------
# UNI
# ----------
X_uni_bull <- dex_bull %>%
  select(delta_UNI_SPOT_VOLUME, delta_UNI_DAU,
         delta_UNI_CAPITAL_EFFICIENCY, delta_UNI_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_uni_bear <- dex_bear %>%
  select(delta_UNI_SPOT_VOLUME, delta_UNI_DAU,
         delta_UNI_CAPITAL_EFFICIENCY, delta_UNI_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(uni_reg_bull, X = X_uni_bull, bg_X = X_uni_bull)), kind = "bar") +
  ggtitle("UNI - Bull Market")
sv_importance(shapviz(kernelshap(uni_reg_bear, X = X_uni_bear, bg_X = X_uni_bear)), kind = "bar") +
  ggtitle("UNI - Bear Market")

# ----------
# AERO
# ----------
X_aero_bull <- dex_bull %>%
  select(delta_AERO_SPOT_VOLUME, delta_AERO_DAU,
         delta_AERO_CAPITAL_EFFICIENCY, delta_AERO_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_aero_bear <- dex_bear %>%
  select(delta_AERO_SPOT_VOLUME, delta_AERO_DAU,
         delta_AERO_CAPITAL_EFFICIENCY, delta_AERO_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(aero_reg_bull, X = X_aero_bull, bg_X = X_aero_bull)), kind = "bar") +
  ggtitle("AERO - Bull Market")
sv_importance(shapviz(kernelshap(aero_reg_bear, X = X_aero_bear, bg_X = X_aero_bear)), kind = "bar") +
  ggtitle("AERO - Bear Market")

# ----------
# CAKE
# ----------
X_cake_bull <- dex_bull %>%
  select(delta_CAKE_SPOT_VOLUME, delta_CAKE_DAU,
         delta_CAKE_CAPITAL_EFFICIENCY, delta_CAKE_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_cake_bear <- dex_bear %>%
  select(delta_CAKE_SPOT_VOLUME, delta_CAKE_DAU,
         delta_CAKE_CAPITAL_EFFICIENCY, delta_CAKE_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(cake_reg_bull, X = X_cake_bull, bg_X = X_cake_bull)), kind = "bar") +
  ggtitle("CAKE - Bull Market")
sv_importance(shapviz(kernelshap(cake_reg_bear, X = X_cake_bear, bg_X = X_cake_bear)), kind = "bar") +
  ggtitle("CAKE - Bear Market")

# ----------
# AAVE
# ----------
X_aave_bull <- lending_bull %>%
  select(delta_AAVE_LENDING_LOANS, delta_AAVE_LENDING_DEPOSITS,
         delta_AAVE_FEES, delta_AAVE_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_aave_bear <- lending_bear %>%
  select(delta_AAVE_LENDING_LOANS, delta_AAVE_LENDING_DEPOSITS,
         delta_AAVE_FEES, delta_AAVE_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(aave_reg_bull, X = X_aave_bull, bg_X = X_aave_bull)), kind = "bar") +
  ggtitle("AAVE - Bull Market")
sv_importance(shapviz(kernelshap(aave_reg_bear, X = X_aave_bear, bg_X = X_aave_bear)), kind = "bar") +
  ggtitle("AAVE - Bear Market")

# ----------
# MORPHO
# ----------
X_morpho_bull <- lending_bull %>%
  select(delta_MORPHO_LENDING_LOANS, delta_MORPHO_LENDING_DEPOSITS,
         delta_MORPHO_FEES, delta_MORPHO_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_morpho_bear <- lending_bear %>%
  select(delta_MORPHO_LENDING_LOANS, delta_MORPHO_LENDING_DEPOSITS,
         delta_MORPHO_FEES, delta_MORPHO_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(morpho_reg_bull, X = X_morpho_bull, bg_X = X_morpho_bull)), kind = "bar") +
  ggtitle("MORPHO - Bull Market")
sv_importance(shapviz(kernelshap(morpho_reg_bear, X = X_morpho_bear, bg_X = X_morpho_bear)), kind = "bar") +
  ggtitle("MORPHO - Bear Market")

# ----------
# HYPE
# ----------
X_hype_bull <- perp_bull %>%
  select(delta_HYPE_PERP_VOLUME, delta_HYPE_PERP_DAU,
         delta_HYPE_REVENUE, delta_HYPE_CHAIN_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_hype_bear <- perp_bear %>%
  select(delta_HYPE_PERP_VOLUME, delta_HYPE_PERP_DAU,
         delta_HYPE_REVENUE, delta_HYPE_CHAIN_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(hype_reg_bull, X = X_hype_bull, bg_X = X_hype_bull)), kind = "bar") +
  ggtitle("HYPE - Bull Market")
sv_importance(shapviz(kernelshap(hype_reg_bear, X = X_hype_bear, bg_X = X_hype_bear)), kind = "bar") +
  ggtitle("HYPE - Bear Market")

# ----------
# ETHFI
# ----------
X_ethfi_bull <- ethfi_bull %>%
  select(delta_ETHFI_CARD_VOLUME, delta_ETHFI_NUM_CARDS,
         delta_ETHFI_FEES, delta_ETHFI_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_ethfi_bear <- ethfi_bear %>%
  select(delta_ETHFI_CARD_VOLUME, delta_ETHFI_NUM_CARDS,
         delta_ETHFI_FEES, delta_ETHFI_TVL,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(ethfi_reg_bull, X = X_ethfi_bull, bg_X = X_ethfi_bull)), kind = "bar") +
  ggtitle("ETHFI - Bull Market")
sv_importance(shapviz(kernelshap(ethfi_reg_bear, X = X_ethfi_bear, bg_X = X_ethfi_bear)), kind = "bar") +
  ggtitle("ETHFI - Bear Market")

# ----------
# PUMP
# ----------
X_pump_bull <- pump_bull %>%
  select(delta_PUMP_TOKENS_GRADUATED, delta_PUMP_BUYBACKS_NATIVE,
         delta_PUMP_REVENUE, delta_PUMP_LAUNCHPAD_VOLUMES,
         crypto_mkt_beta_pca) %>%
  na.omit()

X_pump_bear <- pump_bear %>%
  select(delta_PUMP_TOKENS_GRADUATED, delta_PUMP_BUYBACKS_NATIVE,
         delta_PUMP_REVENUE, delta_PUMP_LAUNCHPAD_VOLUMES,
         crypto_mkt_beta_pca) %>%
  na.omit()

sv_importance(shapviz(kernelshap(pump_reg_bull, X = X_pump_bull, bg_X = X_pump_bull)), kind = "bar") +
  ggtitle("PUMP - Bull Market")
sv_importance(shapviz(kernelshap(pump_reg_bear, X = X_pump_bear, bg_X = X_pump_bear)), kind = "bar") +
  ggtitle("PUMP - Bear Market")


#--------------------
# Percentages Table
# -------------------

shap_pct <- function(model, X) {
  sv <- shapviz(kernelshap(model, X = X, bg_X = X))
  mean_shap <- colMeans(abs(sv$S))
  pct <- round(mean_shap / sum(mean_shap) * 100, 1)
  return(pct)
}

# ----------
# DEX TOKENS
# ----------
uni_pct_bull  <- shap_pct(uni_reg_bull,  X_uni_bull)
uni_pct_bear  <- shap_pct(uni_reg_bear,  X_uni_bear)

aero_pct_bull <- shap_pct(aero_reg_bull, X_aero_bull)
aero_pct_bear <- shap_pct(aero_reg_bear, X_aero_bear)

cake_pct_bull <- shap_pct(cake_reg_bull, X_cake_bull)
cake_pct_bear <- shap_pct(cake_reg_bear, X_cake_bear)

# ----------
# LENDING TOKENS
# ----------
aave_pct_bull  <- shap_pct(aave_reg_bull,  X_aave_bull)
aave_pct_bear  <- shap_pct(aave_reg_bear,  X_aave_bear)

morpho_pct_bull <- shap_pct(morpho_reg_bull, X_morpho_bull)
morpho_pct_bear <- shap_pct(morpho_reg_bear, X_morpho_bear)

# ----------
# PERP / MISC
# ----------
hype_pct_bull <- shap_pct(hype_reg_bull, X_hype_bull)
hype_pct_bear <- shap_pct(hype_reg_bear, X_hype_bear)

ethfi_pct_bull <- shap_pct(ethfi_reg_bull, X_ethfi_bull)
ethfi_pct_bear <- shap_pct(ethfi_reg_bear, X_ethfi_bear)

pump_pct_bull  <- shap_pct(pump_reg_bull,  X_pump_bull)
pump_pct_bear  <- shap_pct(pump_reg_bear,  X_pump_bear)

make_row <- function(token, regime, pct) {
  metrics <- pct[names(pct) != "crypto_mkt_beta_pca"]
  row <- data.frame(
    Token       = token,
    Regime      = regime,
    Market_Beta = unname(pct["crypto_mkt_beta_pca"]),
    row.names   = NULL
  )
  for (nm in names(metrics)) row[[nm]] <- unname(metrics[nm])
  row
}

shap_table <- bind_rows(
  make_row("UNI",    "Bull", uni_pct_bull),
  make_row("UNI",    "Bear", uni_pct_bear),
  make_row("AERO",   "Bull", aero_pct_bull),
  make_row("AERO",   "Bear", aero_pct_bear),
  make_row("CAKE",   "Bull", cake_pct_bull),
  make_row("CAKE",   "Bear", cake_pct_bear),
  make_row("AAVE",   "Bull", aave_pct_bull),
  make_row("AAVE",   "Bear", aave_pct_bear),
  make_row("MORPHO", "Bull", morpho_pct_bull),
  make_row("MORPHO", "Bear", morpho_pct_bear),
  make_row("HYPE",   "Bull", hype_pct_bull),
  make_row("HYPE",   "Bear", hype_pct_bear),
  make_row("ETHFI",  "Bull", ethfi_pct_bull),
  make_row("ETHFI",  "Bear", ethfi_pct_bear),
  make_row("PUMP",   "Bull", pump_pct_bull),
  make_row("PUMP",   "Bear", pump_pct_bear)
)

rownames(shap_table) <- NULL
print(shap_table)

write_xlsx(shap_table, "shap_table.xlsx")

# ----------------------
# HEATMAPS
# ----------------------

long_shap <- shap_table %>%
  pivot_longer(cols = -c(Token, Regime), names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = gsub("^delta_[A-Z]+_", "", Metric),
    Metric = ifelse(Metric == "crypto_mkt_beta_pca", "Market Beta", Metric),
    Metric = gsub("_", " ", Metric),
    Metric = stringr::str_to_title(Metric),
    Metric = gsub("\\bDau\\b",  "DAU",  Metric),
    Metric = gsub("\\bTvl\\b",  "TVL",  Metric),
    Metric = gsub("\\bHip3\\b", "HIP3", Metric)
  ) %>%
  filter(!is.na(Value)) %>%
  mutate(Regime = factor(Regime, levels = c("Bull", "Bear")))

heatmap_theme <- function() {
  theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.title  = element_text(face = "bold"),
      panel.grid  = element_blank()
    )
}

heatmap_scale <- scale_fill_gradientn(
  colors = c("beige", "#079C1B", "#03440C"),
  limits = c(0, 100), name = "SHAP %"
)

order_metrics <- function(metrics) {
  c("Market Beta", setdiff(metrics, "Market Beta"))
}

# ----------
# Individual heatmaps
# ----------
for (tok in c("UNI", "CAKE", "AERO", "AAVE", "MORPHO", "HYPE", "ETHFI", "PUMP")) {
  df <- long_shap %>%
    filter(Token == tok) %>%
    mutate(Metric = factor(Metric, levels = order_metrics(unique(Metric))))
  p <- ggplot(df, aes(x = Metric, y = Regime, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = paste0(round(Value, 1), "%")), size = 3.5) +
    heatmap_scale +
    labs(title = paste(tok, "- SHAP Feature Importance"), x = NULL, y = NULL) +
    heatmap_theme()
  print(p)
}

# ----------
# Sector heatmaps
# ----------

sector_heatmap <- function(tokens, sector_name) {
  row_levels <- rev(as.vector(rbind(paste0(tokens, " (Bull)"), paste0(tokens, " (Bear)"))))
  df <- long_shap %>%
    filter(Token %in% tokens) %>%
    mutate(
      Token_Regime = factor(paste0(Token, " (", Regime, ")"), levels = row_levels),
      Metric       = factor(Metric, levels = order_metrics(unique(Metric)))
    )
  ggplot(df, aes(x = Metric, y = Token_Regime, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = paste0(round(Value, 1), "%")), size = 3.5) +
    heatmap_scale +
    labs(title = paste(sector_name, "- SHAP Feature Importance"), x = NULL, y = NULL) +
    heatmap_theme()
}

sector_heatmap(c("UNI", "CAKE", "AERO"), "DEX")
sector_heatmap(c("AAVE", "MORPHO"),       "Lending")
sector_heatmap(c("HYPE"),                 "Perp")
sector_heatmap(c("ETHFI"),                "ETHFI")
sector_heatmap(c("PUMP"),                 "PUMP")