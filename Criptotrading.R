install_github("daroczig/binancer")

# clean base

rm(list = ls())


# Libraries

library(magrittr)
library(plotly)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(binancer)
library(telegram.bot)


# Begin cycle

repeat {
  
  
  # Source functions 
  
  source("Telegraming.R")
  source("Indication.R")
  
  
  # API keys
  
  key_1 <- "UBaFx31DvB7FwWnR65te9Nhx8ZduX4oNEtiP8GTLVMmM4TCsMUQRGz6TShwToUt8"
  secret_1 <- "0DleR5sMIlbYzk99q3GV0N07OBk4iESgJNrrdiOR8ZWJwK6ywg8xZtSZhmtQNhqH"
  
  
  # Calling API
  
  binance_ping()
  binance_credentials(key = key_1, secret = secret_1)
  
  
  # Telegram fields
  
  token <- "5714075349:AAER6IE28EsAFPAeqroEWcwgTy38RxTu5Pw"
  
  #Inicializar un chat en Telegram con el bot `R_projectDEM_bot`
  bot <- Bot(token = token)
  updates <- bot$get_updates()
  chat_id <- updates[[1L]]$from_chat_id()
  
  
  # Generate new data every minute
  
  # Note that BTCUSDT is giving the indication to use bitcoin and in USD, this key can change as needed.
  
  data <- binance_klines('BTCUSDT', interval = '1m')
  
  # MACD
  
  macd <- MACD(data[,"close"], 12, 26, 9, maType="EMA")
  macd1 <- as.data.frame(macd)
  
  # RSI 
  
  rsi <- RSI(data$close, n = 14, maType = "SMA") 
  rsi1 <- as.data.frame(rsi)
  
  
  # Create a new line for the log file 
  
  df1 <- indication(macd1, rsi1)
  status <- df1$decision[50]
  
  
  if (status == "Buy"){
    
    balance <- df1$price[50]
    GanPer <- 0
    results <- cbind(df1[50,], balance, GanPer)
    
    # Save to CSV
    
    write.table(results, file="results.csv", append = T, sep = ',', row.names=F, col.names=F)
    
    # Send Telegram
    
    text <- "'Transaction notice' , 'You bought bitcoin!'"
    bot$sendMessage(chat_id, text = text)
    
    while(status == "Buy" | status == "Wait"){
      
      data <- binance_klines('BTCUSDT', interval = '1m')
      macd <- MACD(data[,"close"], 12, 26, 9, maType="EMA")
      macd1 <- as.data.frame(macd)
      rsi <- RSI(data$close, n = 14, maType = "SMA") 
      rsi1 <- as.data.frame(rsi)
      df1 <- indica_entrada(macd1, rsi1)
      status <- df1$decision[50]
      
    }
    
    if (status == "Sell"){
      
      balance <- df1$precio[50] - balance
      GanPer <- balance
      results <- cbind(df1[50,], balance, GanPer)
      
      # Save to CSV
      
      write.table(results, file="results.csv", append = T, sep = ',', row.names=F, col.names=F)
      
      # Send email
      
      text <- "'Transaction notice' , 'You sold bitcoin!'"
      bot$sendMessage(chat_id, text = text)
      
      while(status == "Sell"){
        
        data <- binance_klines('BTCUSDT', interval = '1m')
        macd <- MACD(data[,"close"], 12, 26, 9, maType="EMA")
        macd1 <- as.data.frame(macd)
        rsi <- RSI(data$close, n = 14, maType = "SMA") 
        rsi1 <- as.data.frame(rsi)
        df1 <- indica_entrada(macd1, rsi1)
        status <- df1$decision[50]
        
      }
    } 
    
  } else {
    
    write.table(df1[50,], file="df1.csv", append = T, sep = ',', row.names=F, col.names=F)
    
  }
  
  Sys.sleep(time=60)
  
}

# Profit calculation

results <- read.csv('results.csv', header = F)

total <- sum(results$V7) + results[1,6]

Concept <- c('Init_Invest', 'Total', 'Profit')

Values <- c(results[1,6], total, total - results[1,6])

Values <- data.frame(Concept, Values)

Values