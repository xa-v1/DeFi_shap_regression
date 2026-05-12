library(googlesheets4)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)

#-------------------
# LOAD MARKET BETA
#-------------------

btc <- tq_get("BTC-USD", from = "2024-12-01", to = Sys.Date()) %>%
  select(symbol, date, close) %>%
  arrange(date) %>%
  mutate(btc_return = log(close / dplyr::lag(close)))

eth <- tq_get("ETH-USD", from = "2024-12-01", to = Sys.Date()) %>%
  select(symbol, date, close) %>%
  arrange(date) %>%
  mutate(eth_return = log(close / dplyr::lag(close)))

sol <- tq_get("SOL-USD", from = "2024-12-01", to = Sys.Date()) %>%
  select(symbol, date, close) %>%
  arrange(date) %>%
  mutate(sol_return = log(close / dplyr::lag(close)))

#--------------
# MKT BETA PCA
#--------------

crypto_returns <- btc %>%
  select(date, btc_return) %>%
  left_join(eth %>% select(date, eth_return), by = "date") %>%
  left_join(sol %>% select(date, sol_return), by = "date") %>%
  drop_na() 

pca <- prcomp(crypto_returns %>% select(btc_return, eth_return, sol_return), 
              scale = TRUE) 

summary(pca)

crypto_returns <- crypto_returns %>%
  mutate(crypto_mkt_beta_pca = pca$x[,1])

#-----------------------
# ClEAN AND MUTATE DEX
#-----------------------

dex_metrics = read_sheet("https://docs.google.com/spreadsheets/d/1ZgRL3REaUXUwuh__WdwXBbP6aDBopsTJotOHJt37ydg/edit?gid=0#gid=0")
dex_metrics = dex_metrics[-1,]
dex_metrics <- dex_metrics %>%
  rename(
    UNI_date        = 2,
    UNI_price       = 3,
    UNI_SPOT_VOLUME        = 4,
    UNI_DAU                = 5,
    UNI_CAPITAL_EFFICIENCY = 6,
    UNI_TVL                = 7,

    CAKE_date               = 9,
    CAKE_price              = 10,
    CAKE_SPOT_VOLUME        = 11,
    CAKE_DAU                = 12,
    CAKE_CAPITAL_EFFICIENCY = 13,
    CAKE_TVL                = 14,

    AERO_date               = 16,
    AERO_price              = 17,
    AERO_SPOT_VOLUME        = 18,
    AERO_DAU                = 19,
    AERO_CAPITAL_EFFICIENCY = 20,
    AERO_TVL                = 21
  )
dex_metrics <- dex_metrics %>%
  select(-1, -8, -15)
dex_metrics <- dex_metrics %>%
  mutate(
    UNI_price              = as.numeric(sapply(UNI_price,              function(x) ifelse(is.null(x), NA, x))),
    UNI_SPOT_VOLUME        = as.numeric(sapply(UNI_SPOT_VOLUME,        function(x) ifelse(is.null(x), NA, x))),
    UNI_DAU                = as.numeric(sapply(UNI_DAU,                function(x) ifelse(is.null(x), NA, x))),
    UNI_CAPITAL_EFFICIENCY = as.numeric(sapply(UNI_CAPITAL_EFFICIENCY, function(x) ifelse(is.null(x), NA, x))),
    UNI_TVL                = as.numeric(sapply(UNI_TVL,                function(x) ifelse(is.null(x), NA, x))),
    CAKE_price              = as.numeric(sapply(CAKE_price,              function(x) ifelse(is.null(x), NA, x))),
    CAKE_SPOT_VOLUME        = as.numeric(sapply(CAKE_SPOT_VOLUME,        function(x) ifelse(is.null(x), NA, x))),
    CAKE_DAU                = as.numeric(sapply(CAKE_DAU,                function(x) ifelse(is.null(x), NA, x))),
    CAKE_CAPITAL_EFFICIENCY = as.numeric(sapply(CAKE_CAPITAL_EFFICIENCY, function(x) ifelse(is.null(x), NA, x))),
    CAKE_TVL                = as.numeric(sapply(CAKE_TVL,                function(x) ifelse(is.null(x), NA, x))),
    AERO_price              = as.numeric(sapply(AERO_price,              function(x) ifelse(is.null(x), NA, x))),
    AERO_SPOT_VOLUME        = as.numeric(sapply(AERO_SPOT_VOLUME,        function(x) ifelse(is.null(x), NA, x))),
    AERO_DAU                = as.numeric(sapply(AERO_DAU,                function(x) ifelse(is.null(x), NA, x))),
    AERO_CAPITAL_EFFICIENCY = as.numeric(sapply(AERO_CAPITAL_EFFICIENCY, function(x) ifelse(is.null(x), NA, x))),
    AERO_TVL                = as.numeric(sapply(AERO_TVL,                function(x) ifelse(is.null(x), NA, x)))
  ) %>%
  filter(complete.cases(.))
dex_metrics <- dex_metrics %>%
  mutate(
    UNI_date        = as.Date(UNI_date,  format = "%m/%d/%Y"),
    UNI_price              = as.numeric(unlist(UNI_price)),
    UNI_SPOT_VOLUME        = as.numeric(unlist(UNI_SPOT_VOLUME)),
    UNI_DAU                = as.numeric(unlist(UNI_DAU)),
    UNI_CAPITAL_EFFICIENCY = as.numeric(unlist(UNI_CAPITAL_EFFICIENCY)),
    UNI_TVL                = as.numeric(unlist(UNI_TVL))
  )
dex_metrics <- dex_metrics %>%
  mutate(
    CAKE_date               = as.Date(CAKE_date, format = "%m/%d/%Y"),
    CAKE_price              = as.numeric(unlist(CAKE_price)),
    CAKE_SPOT_VOLUME        = as.numeric(unlist(CAKE_SPOT_VOLUME)),
    CAKE_DAU                = as.numeric(unlist(CAKE_DAU)),
    CAKE_CAPITAL_EFFICIENCY = as.numeric(unlist(CAKE_CAPITAL_EFFICIENCY)),
    CAKE_TVL                = as.numeric(unlist(CAKE_TVL))
  )
dex_metrics <- dex_metrics %>%
  mutate(
    AERO_date               = as.Date(AERO_date, format = "%m/%d/%Y"),
    AERO_price              = as.numeric(unlist(AERO_price)),
    AERO_SPOT_VOLUME        = as.numeric(unlist(AERO_SPOT_VOLUME)),
    AERO_DAU                = as.numeric(unlist(AERO_DAU)),
    AERO_CAPITAL_EFFICIENCY = as.numeric(unlist(AERO_CAPITAL_EFFICIENCY)),
    AERO_TVL                = as.numeric(unlist(AERO_TVL))
  )

dex_metrics = dex_metrics %>%
  mutate(UNI_return  = log(UNI_price  / dplyr::lag(UNI_price)))

dex_metrics = dex_metrics %>%
  mutate(CAKE_return = log(CAKE_price / dplyr::lag(CAKE_price)))

dex_metrics = dex_metrics %>%
  mutate(AERO_return = log(AERO_price / dplyr::lag(AERO_price)))

dex_metrics = dex_metrics %>%
  mutate(
    delta_AERO_SPOT_VOLUME        = log(AERO_SPOT_VOLUME        / dplyr::lag(AERO_SPOT_VOLUME)),
    delta_AERO_DAU                = log(AERO_DAU                / dplyr::lag(AERO_DAU)),
    delta_AERO_CAPITAL_EFFICIENCY = log(AERO_CAPITAL_EFFICIENCY / dplyr::lag(AERO_CAPITAL_EFFICIENCY)),
    delta_AERO_TVL                = log(AERO_TVL                / dplyr::lag(AERO_TVL)),
    delta_UNI_SPOT_VOLUME         = log(UNI_SPOT_VOLUME         / dplyr::lag(UNI_SPOT_VOLUME)),
    delta_UNI_DAU                 = log(UNI_DAU                 / dplyr::lag(UNI_DAU)),
    delta_UNI_CAPITAL_EFFICIENCY  = log(UNI_CAPITAL_EFFICIENCY  / dplyr::lag(UNI_CAPITAL_EFFICIENCY)),
    delta_UNI_TVL                 = log(UNI_TVL                 / dplyr::lag(UNI_TVL)),
    delta_CAKE_SPOT_VOLUME        = log(CAKE_SPOT_VOLUME        / dplyr::lag(CAKE_SPOT_VOLUME)),
    delta_CAKE_DAU                = log(CAKE_DAU                / dplyr::lag(CAKE_DAU)),
    delta_CAKE_CAPITAL_EFFICIENCY = log(CAKE_CAPITAL_EFFICIENCY / dplyr::lag(CAKE_CAPITAL_EFFICIENCY)),
    delta_CAKE_TVL                = log(CAKE_TVL                / dplyr::lag(CAKE_TVL)),
    )

dex_metrics <- dex_metrics %>%
  mutate(date = UNI_date) %>%
  select(date, everything(), -UNI_date, -CAKE_date, -AERO_date)

dex_metrics = dex_metrics %>%
  left_join(crypto_returns %>% select(date, crypto_mkt_beta_pca), by = "date") %>%
  drop_na()

#--------------------------
# CLEAN AND MUTATE LENDING
#--------------------------

lending_metrics = read_sheet("https://docs.google.com/spreadsheets/d/1L3YDELLrqvHPAFSk692ipbtFUfNkegxRrXojh877O_c/edit?gid=0#gid=0")
lending_metrics = lending_metrics[-1,]
lending_metrics <- lending_metrics %>%
  rename(
    AAVE_date        = 2,
    AAVE_price       = 3,
    AAVE_LENDING_LOANS    = 4,
    AAVE_LENDING_DEPOSITS = 5,
    AAVE_FEES             = 6,
    AAVE_TVL              = 7,

    MORPHO_date             = 10,
    MORPHO_price            = 11,
    MORPHO_LENDING_LOANS    = 12,
    MORPHO_LENDING_DEPOSITS = 13,
    MORPHO_FEES             = 14,
    MORPHO_TVL              = 15,
)
lending_metrics <- lending_metrics %>%
  select(-1, -8, -9)
lending_metrics <- lending_metrics %>%
  mutate(
    AAVE_price            = as.numeric(sapply(AAVE_price,            function(x) ifelse(is.null(x), NA, x))),
    AAVE_LENDING_LOANS    = as.numeric(sapply(AAVE_LENDING_LOANS,    function(x) ifelse(is.null(x), NA, x))),
    AAVE_LENDING_DEPOSITS = as.numeric(sapply(AAVE_LENDING_DEPOSITS, function(x) ifelse(is.null(x), NA, x))),
    AAVE_FEES             = as.numeric(sapply(AAVE_FEES,             function(x) ifelse(is.null(x), NA, x))),
    AAVE_TVL              = as.numeric(sapply(AAVE_TVL,              function(x) ifelse(is.null(x), NA, x))),
    MORPHO_price            = as.numeric(sapply(MORPHO_price,            function(x) ifelse(is.null(x), NA, x))),
    MORPHO_LENDING_LOANS    = as.numeric(sapply(MORPHO_LENDING_LOANS,    function(x) ifelse(is.null(x), NA, x))),
    MORPHO_LENDING_DEPOSITS = as.numeric(sapply(MORPHO_LENDING_DEPOSITS, function(x) ifelse(is.null(x), NA, x))),
    MORPHO_FEES             = as.numeric(sapply(MORPHO_FEES,             function(x) ifelse(is.null(x), NA, x))),
    MORPHO_TVL              = as.numeric(sapply(MORPHO_TVL,              function(x) ifelse(is.null(x), NA, x)))
  ) %>%
  filter(complete.cases(.))
lending_metrics <- lending_metrics %>%
  mutate(
    AAVE_date       = as.Date(AAVE_date, format = "%m/%d/%Y"),
    AAVE_price            = as.numeric(unlist(AAVE_price)),
    AAVE_LENDING_LOANS    = as.numeric(unlist(AAVE_LENDING_LOANS)),
    AAVE_LENDING_DEPOSITS = as.numeric(unlist(AAVE_LENDING_DEPOSITS)),
    AAVE_FEES             = as.numeric(unlist(AAVE_FEES)),
    AAVE_TVL              = as.numeric(unlist(AAVE_TVL))
  )
lending_metrics <- lending_metrics %>%
  mutate(
    MORPHO_date             = as.Date(MORPHO_date, format = "%m/%d/%Y"),
    MORPHO_price            = as.numeric(unlist(MORPHO_price)),
    MORPHO_LENDING_LOANS    = as.numeric(unlist(MORPHO_LENDING_LOANS)),
    MORPHO_LENDING_DEPOSITS = as.numeric(unlist(MORPHO_LENDING_DEPOSITS)),
    MORPHO_FEES             = as.numeric(unlist(MORPHO_FEES)),
    MORPHO_TVL              = as.numeric(unlist(MORPHO_TVL))
  )
lending_metrics <- lending_metrics %>%
  mutate(AAVE_return = log(AAVE_price / dplyr::lag(AAVE_price)))
lending_metrics <- lending_metrics %>%
  mutate(MORPHO_return = log(MORPHO_price / dplyr::lag(MORPHO_price)))

lending_metrics = lending_metrics %>%
  mutate(
    delta_AAVE_LENDING_LOANS    = log(AAVE_LENDING_LOANS    / dplyr::lag(AAVE_LENDING_LOANS)),
    delta_AAVE_LENDING_DEPOSITS = log(AAVE_LENDING_DEPOSITS / dplyr::lag(AAVE_LENDING_DEPOSITS)),
    delta_AAVE_FEES             = log(AAVE_FEES             / dplyr::lag(AAVE_FEES)),
    delta_AAVE_TVL              = log(AAVE_TVL              / dplyr::lag(AAVE_TVL)),
    delta_MORPHO_LENDING_LOANS    = log(MORPHO_LENDING_LOANS    / dplyr::lag(MORPHO_LENDING_LOANS)),
    delta_MORPHO_LENDING_DEPOSITS = log(MORPHO_LENDING_DEPOSITS / dplyr::lag(MORPHO_LENDING_DEPOSITS)),
    delta_MORPHO_FEES             = log(MORPHO_FEES             / dplyr::lag(MORPHO_FEES)),
    delta_MORPHO_TVL              = log(MORPHO_TVL              / dplyr::lag(MORPHO_TVL)),
  )

lending_metrics <- lending_metrics %>%
  mutate(date = AAVE_date) %>%
  select(date, everything(), -AAVE_date, -MORPHO_date)

lending_metrics = lending_metrics %>%
  left_join(crypto_returns %>% select(date, crypto_mkt_beta_pca), by = "date") %>%
  drop_na()

#--------------------------
# ClEAN AND MUTATE PERPS
#--------------------------

perp_metrics = read_sheet("https://docs.google.com/spreadsheets/d/1u7Osz3HJz5lyQpHR3HKD2YKd2X6cw5odf2FuT_0kzBE/edit?gid=0#gid=0")
perp_metrics = perp_metrics[-1,]
perp_metrics <- perp_metrics %>%
  rename(
    HYPE_date        = 2,
    HYPE_price       = 3,
    HYPE_PERP_VOLUME = 4,
    HYPE_PERP_DAU    = 5,
    HYPE_REVENUE    = 6,
    HYPE_CHAIN_TVL   = 7,
)
perp_metrics <- perp_metrics %>%
  select(-1)
perp_metrics <- perp_metrics %>%
  mutate(
    HYPE_price       = as.numeric(sapply(HYPE_price, function(x) ifelse(is.null(x), NA, x))),
    HYPE_PERP_VOLUME = as.numeric(sapply(HYPE_PERP_VOLUME, function(x) ifelse(is.null(x), NA, x))),
    HYPE_PERP_DAU    = as.numeric(sapply(HYPE_PERP_DAU, function(x) ifelse(is.null(x), NA, x))),
    HYPE_REVENUE     = as.numeric(sapply(HYPE_REVENUE, function(x) ifelse(is.null(x), NA, x))),
    HYPE_CHAIN_TVL  = as.numeric(sapply(HYPE_CHAIN_TVL, function(x) ifelse(is.null(x), NA, x)))
  ) %>%
  filter(complete.cases(.))
perp_metrics <- perp_metrics %>%
  mutate(
    HYPE_date       = as.Date(HYPE_date, format = "%m/%d/%Y"),
    HYPE_price                      = as.numeric(unlist(HYPE_price)),
    HYPE_PERP_VOLUME = as.numeric(unlist(HYPE_PERP_VOLUME)),
    HYPE_PERP_DAU                   = as.numeric(unlist(HYPE_PERP_DAU)),
    HYPE_REVENUE                    = as.numeric(unlist(HYPE_REVENUE)),
    HYPE_CHAIN_TVL                  = as.numeric(unlist(HYPE_CHAIN_TVL))
  )
perp_metrics <- perp_metrics %>%
  mutate(HYPE_return = log(HYPE_price / dplyr::lag(HYPE_price)))

perp_metrics = perp_metrics %>%
  mutate(
    delta_HYPE_PERP_VOLUME = log(HYPE_PERP_VOLUME / dplyr::lag(HYPE_PERP_VOLUME)),
    delta_HYPE_PERP_DAU                   = log(HYPE_PERP_DAU                   / dplyr::lag(HYPE_PERP_DAU)),
    delta_HYPE_REVENUE                    = log(HYPE_REVENUE                    / dplyr::lag(HYPE_REVENUE)),
    delta_HYPE_CHAIN_TVL                  = log(HYPE_CHAIN_TVL                  / dplyr::lag(HYPE_CHAIN_TVL)),
  )

perp_metrics <- perp_metrics %>%
  mutate(date = HYPE_date) %>%
  select(date, everything(), -HYPE_date)

perp_metrics = perp_metrics %>%
  left_join(crypto_returns %>% select(date, crypto_mkt_beta_pca), by = "date") %>%
  drop_na()

#--------------------------
# CLEAN AND MUTATE MISC
#--------------------------

misc_raw = read_sheet("https://docs.google.com/spreadsheets/d/12jaV8ef3zi3BLnRQ4aLfPpuACVAnxI3-O26oViQp9OI/edit?gid=0#gid=0")
misc_raw = misc_raw[-1,]

# ETHFI
ethfi_metrics <- misc_raw %>%
  select(2:7) %>%
  rename(
    ETHFI_date        = 1,
    ETHFI_price       = 2,
    ETHFI_CARD_VOLUME = 3,
    ETHFI_NUM_CARDS   = 4,
    ETHFI_FEES        = 5,
    ETHFI_TVL         = 6
  )
ethfi_metrics <- ethfi_metrics %>%
  mutate(
    ETHFI_price       = as.numeric(sapply(ETHFI_price,       function(x) ifelse(is.null(x), NA, x))),
    ETHFI_CARD_VOLUME = as.numeric(sapply(ETHFI_CARD_VOLUME, function(x) ifelse(is.null(x), NA, x))),
    ETHFI_NUM_CARDS   = as.numeric(sapply(ETHFI_NUM_CARDS,   function(x) ifelse(is.null(x), NA, x))),
    ETHFI_FEES        = as.numeric(sapply(ETHFI_FEES,        function(x) ifelse(is.null(x), NA, x))),
    ETHFI_TVL         = as.numeric(sapply(ETHFI_TVL,         function(x) ifelse(is.null(x), NA, x)))
  ) %>%
  filter(complete.cases(.))
ethfi_metrics <- ethfi_metrics %>%
  mutate(
    ETHFI_date        = as.Date(ETHFI_date, format = "%m/%d/%Y"),
    ETHFI_price       = as.numeric(unlist(ETHFI_price)),
    ETHFI_CARD_VOLUME = as.numeric(unlist(ETHFI_CARD_VOLUME)),
    ETHFI_NUM_CARDS   = as.numeric(unlist(ETHFI_NUM_CARDS)),
    ETHFI_FEES        = as.numeric(unlist(ETHFI_FEES)),
    ETHFI_TVL         = as.numeric(unlist(ETHFI_TVL))
  )
ethfi_metrics <- ethfi_metrics %>%
  mutate(ETHFI_return = log(ETHFI_price / dplyr::lag(ETHFI_price)))
ethfi_metrics <- ethfi_metrics %>%
  mutate(
    delta_ETHFI_CARD_VOLUME = log(ETHFI_CARD_VOLUME / dplyr::lag(ETHFI_CARD_VOLUME)),
    delta_ETHFI_NUM_CARDS   = log(ETHFI_NUM_CARDS   / dplyr::lag(ETHFI_NUM_CARDS)),
    delta_ETHFI_FEES        = log(ETHFI_FEES        / dplyr::lag(ETHFI_FEES)),
    delta_ETHFI_TVL         = log(ETHFI_TVL         / dplyr::lag(ETHFI_TVL)),
  )
ethfi_metrics <- ethfi_metrics %>%
  mutate(date = ETHFI_date) %>%
  select(date, everything(), -ETHFI_date)
ethfi_metrics <- ethfi_metrics %>%
  left_join(crypto_returns %>% select(date, crypto_mkt_beta_pca), by = "date") %>%
  drop_na()

# PUMP
pump_metrics <- misc_raw %>%
  select(10:15) %>%
  rename(
    PUMP_date              = 1,
    PUMP_price             = 2,
    PUMP_TOKENS_GRADUATED  = 3,
    PUMP_BUYBACKS_NATIVE   = 4,
    PUMP_REVENUE           = 5,
    PUMP_LAUNCHPAD_VOLUMES = 6
  )
pump_metrics <- pump_metrics %>%
  mutate(
    PUMP_price             = as.numeric(sapply(PUMP_price,             function(x) ifelse(is.null(x), NA, x))),
    PUMP_TOKENS_GRADUATED  = as.numeric(sapply(PUMP_TOKENS_GRADUATED,  function(x) ifelse(is.null(x), NA, x))),
    PUMP_BUYBACKS_NATIVE   = as.numeric(sapply(PUMP_BUYBACKS_NATIVE,   function(x) ifelse(is.null(x), NA, x))),
    PUMP_REVENUE           = as.numeric(sapply(PUMP_REVENUE,           function(x) ifelse(is.null(x), NA, x))),
    PUMP_LAUNCHPAD_VOLUMES = as.numeric(sapply(PUMP_LAUNCHPAD_VOLUMES, function(x) ifelse(is.null(x), NA, x)))
  ) %>%
  filter(complete.cases(.))
pump_metrics <- pump_metrics %>%
  mutate(
    PUMP_date              = as.Date(PUMP_date, format = "%m/%d/%Y"),
    PUMP_price             = as.numeric(unlist(PUMP_price)),
    PUMP_TOKENS_GRADUATED  = as.numeric(unlist(PUMP_TOKENS_GRADUATED)),
    PUMP_BUYBACKS_NATIVE   = as.numeric(unlist(PUMP_BUYBACKS_NATIVE)),
    PUMP_REVENUE           = as.numeric(unlist(PUMP_REVENUE)),
    PUMP_LAUNCHPAD_VOLUMES = as.numeric(unlist(PUMP_LAUNCHPAD_VOLUMES))
  )
pump_metrics <- pump_metrics %>%
  mutate(PUMP_return = log(PUMP_price / dplyr::lag(PUMP_price)))
pump_metrics <- pump_metrics %>%
  mutate(
    delta_PUMP_TOKENS_GRADUATED  = log(PUMP_TOKENS_GRADUATED  / dplyr::lag(PUMP_TOKENS_GRADUATED)),
    delta_PUMP_BUYBACKS_NATIVE   = log((PUMP_BUYBACKS_NATIVE + 1) / (dplyr::lag(PUMP_BUYBACKS_NATIVE) + 1)),
    delta_PUMP_REVENUE           = log(PUMP_REVENUE +1 )          / (dplyr::lag(PUMP_REVENUE)+1),
    delta_PUMP_LAUNCHPAD_VOLUMES = log(PUMP_LAUNCHPAD_VOLUMES + 1) / (dplyr::lag(PUMP_LAUNCHPAD_VOLUMES)+1),
  )
pump_metrics <- pump_metrics %>%
  mutate(date = PUMP_date) %>%
  select(date, everything(), -PUMP_date)
pump_metrics <- pump_metrics %>%
  left_join(crypto_returns %>% select(date, crypto_mkt_beta_pca), by = "date") %>%
  drop_na()

#-----------------------
# BULL AND BEAR MARKETS
#-----------------------

btc_ma <- tq_get("BTC-USD",
                 from = "2024-05-16",
                 to   = Sys.Date(),
                 get  = "stock.prices") %>%
  fill(adjusted, .direction = "down") %>%
  tq_mutate(select     = adjusted,
            mutate_fun = SMA,
            n          = 200) %>%
  select(date, adjusted, SMA) %>%
  rename(BTC_price = adjusted,
         BTC_200ma = SMA) %>%
  mutate(bull_market = BTC_price > BTC_200ma)

dex_metrics <- dex_metrics %>%
  left_join(btc_ma %>% select(date, BTC_200ma, bull_market), by = "date")

lending_metrics <- lending_metrics %>%
  left_join(btc_ma %>% select(date, BTC_200ma, bull_market), by = "date")

ethfi_metrics <- ethfi_metrics %>%
  left_join(btc_ma %>% select(date, BTC_200ma, bull_market), by = "date")

pump_metrics <- pump_metrics %>%
  left_join(btc_ma %>% select(date, BTC_200ma, bull_market), by = "date")

perp_metrics <- perp_metrics %>%
  left_join(btc_ma %>% select(date, BTC_200ma, bull_market), by = "date")

dex_bull <- dex_metrics %>% filter(bull_market == TRUE)
dex_bear <- dex_metrics %>% filter(bull_market == FALSE)

ethfi_bull <- ethfi_metrics %>% filter(bull_market == TRUE)
ethfi_bear <- ethfi_metrics %>% filter(bull_market == FALSE)

pump_bull <- pump_metrics %>% filter(bull_market == TRUE)
pump_bear <- pump_metrics %>% filter(bull_market == FALSE)

lending_bull <- lending_metrics %>% filter(bull_market == TRUE)
lending_bear <- lending_metrics %>% filter(bull_market == FALSE)

perp_bull <- perp_metrics %>% filter(bull_market == TRUE)
perp_bear <- perp_metrics %>% filter(bull_market == FALSE)