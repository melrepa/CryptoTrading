
library(magrittr)
library(plotly)
library(quantmod)
library(dplyr)
library(DescTools)
library(devtools)
install_github("daroczig/binancer")
library(binancer)

key_1 <- "UBaFx31DvB7FwWnR65te9Nhx8ZduX4oNEtiP8GTLVMmM4TCsMUQRGz6TShwToUt8"
secret_1 <- "0DleR5sMIlbYzk99q3GV0N07OBk4iESgJNrrdiOR8ZWJwK6ywg8xZtSZhmtQNhqH"

binance_ping()
binance_credentials(key = key_1, secret = secret_1)



data <- binance_klines('BTCUSDT', interval = '1m')

precioBTC <- binance_coins_prices(unit = "USDT")  %>%
  filter(symbol == "BTC")
precioBTC$usd

head(data)
str(data)
x <- data$open_time
y <- data %>% dplyr::select(open, high, low, close) %>% as.matrix()

# Candlestick

data %>% plot_ly(x = data$open_time, type="candlestick",
                 open = data$open, close = data$close,
                 high = data$high, low = data$low) %>%
  layout(title = "Candlestick Chart")

# MACD

macd <- MACD(data[,"close"], 12, 26, 9, maType="EMA" )
macd

# MACD as data frame

macd1 <- as.data.frame(macd)

# RSI 

rsi <- RSI(data$close, n = 14, maType = "SMA")

# RSI as data frame

rsi1 <- as.data.frame(rsi)

# Candlestick with MACD and RSI

chartSeries(data,theme=chartTheme('black'))
addMACD(fast=12,slow=26,signal=9,type="EMA")
addRSI(data$close, n = 14, maType = "SMA")



