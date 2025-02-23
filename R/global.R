# Required packages
library(plotly)
library(quantmod)
library(tidyverse)
library(fredr)

# Set FRED API key
fredr_set_key("09726ff4acb374b79923e448785a3a51")

# Set time period
start_date <- Sys.Date() - years(50)

# Functions
yahoo_data <- function(ticker) {
  data <- getSymbols(ticker, src = "yahoo", from = start_date, env = NULL)
  data.frame(
    date = index(data),
    price = as.numeric(Cl(data))
  )
}

fred_data <- function(ticker) {
  fredr(
    series_id = ticker,
    observation_start = start_date
  ) %>%
    select(date, price = value)
}

convert_to_usd <- function(index_data, currency_data, currency_mult = 1) {
  # Ensure dates are in Date format
  index_data$date <- as.Date(index_data$date)
  currency_data$date <- as.Date(currency_data$date)
  # Join index data with currency data
  converted_data <- index_data %>%
    left_join(currency_data, by = "date", suffix = c("_index", "_currency")) %>%
    # If no currency data for a date, carry forward last known rate
    fill(price_currency) %>%
    # Calculate USD value
    mutate(price = price_index * price_currency * currency_mult) %>%
    select(date, price)
  return(converted_data)
}

# STOCKS (Unit: USD per share)
stock_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "KO", "WMT", "JPM", "BRK-B", "V", "DIS")
stock_data <- map(stock_tickers, yahoo_data) %>% set_names(stock_tickers)

# CURRENCIES vs USD
currency_series <- c(
  "EURUSD=X", "JPYUSD=X", "GBPUSD=X", "CNYUSD=X", "CADUSD=X", 
  "SGDUSD=X", "CHFUSD=X", "BRLUSD=X", "MXNUSD=X", "INRUSD=X", "HKDUSD=X"
)
currency_data <- map(currency_series, yahoo_data) %>% set_names(currency_series)
currency_data$USD <- tibble(
  date = seq(from = start_date, to = Sys.Date(), by = "day"),
  price = 1
)

# MARKET INDICES
index_tickers <- c("^GSPC", "^DJI", "^IXIC", "^FTSE", "^N225", "^HSI", "VNQ")
index_data <- map(index_tickers, yahoo_data) %>% set_names(index_tickers)

# Convert indices to USD
index_data[["^FTSE"]] <- convert_to_usd(index_data[["^FTSE"]], currency_data[["GBPUSD=X"]])
index_data[["^N225"]] <- convert_to_usd(index_data[["^N225"]], currency_data[["JPYUSD=X"]])
currency_data[["HKDUSD=X"]] <- yahoo_data("HKDUSD=X")
index_data[["^HSI"]] <- convert_to_usd(index_data[["^HSI"]], currency_data[["HKDUSD=X"]])

# CRYPTO AND PRECIOUS METALS
crypto_series <- c("BTC-USD", "ETH-USD", "GLD", "SLV", "PPLT", "PALL")
crypto_data <- map(crypto_series, yahoo_data) %>% set_names(crypto_series)

# ENERGY & INDUSTRY
commodity_series <- c(
  "DCOILWTICO", "DHHNGSP", "DCOILBRENTEU", "PCOALAUUSDM", 
  "PALUMUSDM", "PNICKUSDM"
)
commodity_data <- map(commodity_series, fred_data) %>% set_names(commodity_series)

# AGRICULTURAL COMMODITIES
agri_series <- c(
  "PWHEAMTUSDM", "PMAIZMTUSDM", "PSOYBUSDM", 
  "APU0000711211", "APU0000703112", "APU0000708111"
)
agri_data <- map(agri_series, fred_data) %>% set_names(agri_series)

# ECONOMIC INDICES
economic_series <- c(
  "CPIAUCSL", "GDP", "BOGMBASE", "GFDEBTN", 
  "M2SL", "MSPUS", "ASPUS"
)
economic_data <- map(economic_series, fred_data) %>% set_names(economic_series)

# Define naming maps
names_stocks <- c(
  AAPL = "Apple", MSFT = "Microsoft", GOOGL = "Google", 
  AMZN = "Amazon", META = "Meta", NVDA = "Nvidia",
  KO = "Coca_Cola", WMT = "Walmart", JPM = "JPMorgan",
  `BRK-B` = "Berkshire_Hathaway", V = "Visa", DIS = "Disney"
)

names_index <- c(
  `^GSPC` = "SP500", `^DJI` = "Dow_Jones", `^IXIC` = "NASDAQ_Composite",
  `^FTSE` = "FTSE100", `^N225` = "Nikkei225", `^HSI` = "HangSeng",
  VNQ = "Vanguard_Real_Estate_ETF"
)

names_crypto <- c(
  `BTC-USD` = "Bitcoin", `ETH-USD` = "Ethereum",
  GLD = "Gold", SLV = "Silver", PPLT = "Platinum", PALL = "Palladium"
)

names_commodity <- c(
  DCOILWTICO = "WTI_Crude_Oil", DHHNGSP = "Natural_Gas",
  DCOILBRENTEU = "Brent_Crude", PCOALAUUSDM = "Coal",
  PALUMUSDM = "Aluminum", PNICKUSDM = "Nickel"
)

names_agri <- c(
  PWHEAMTUSDM = "Wheat", PMAIZMTUSDM = "Corn", PSOYBUSDM = "Soybean",
  APU0000711211 = "Potato", APU0000703112 = "Beef", APU0000708111 = "Eggs"
)

names_currency <- c(
  `EURUSD=X` = "Euro", `JPYUSD=X` = "Japanese Yen", 
  `GBPUSD=X` = "British Pound", `CNYUSD=X` = "Chinese Yuan",
  `CADUSD=X` = "Canadian Dollar", `SGDUSD=X` = "Singapore Dollar",
  `CHFUSD=X` = "Swiss Franc", `BRLUSD=X` = "Brazilian Real",
  `MXNUSD=X` = "Mexican Peso", `INRUSD=X` = "Indian Rupee",
  USD = "USD"
)

names_economy <- c(
  CPIAUCSL = "Consumer_Price_Index",
  GDP = "GDP (Billions USD)",
  BOGMBASE = "Monetary Base (Billions USD)",
  GFDEBTN = "Federal Debt (Millions USD)",
  M2SL = "M2 Money Supply (Billions USD)",
  MSPUS = "Median_House_Price",
  ASPUS = "Average_House_Price"
)

# Rename the list elements
names(stock_data) <- names_stocks[names(stock_data)]
names(index_data) <- names_index[names(index_data)]
names(crypto_data) <- names_crypto[names(crypto_data)]
names(commodity_data) <- names_commodity[names(commodity_data)]
names(agri_data) <- names_agri[names(agri_data)]
names(currency_data) <- names_currency[names(currency_data)]
names(economic_data) <- names_economy[names(economic_data)]

# Combine all data into a single list
all_data <- c(
  stock_data, 
  index_data, 
  crypto_data, 
  commodity_data, 
  agri_data, 
  currency_data, 
  economic_data
)

# Choices for UI
select_choice <- list(
  "stocks" = names(stock_data),
  "International Indices" = names(index_data),
  "Crypto + precious metals" = names(crypto_data),
  "Energy + Industry" = names(commodity_data),
  "Agriculture" = names(agri_data),
  "Currencies" = names(currency_data),
  "Economic Indices" = names(economic_data)
)