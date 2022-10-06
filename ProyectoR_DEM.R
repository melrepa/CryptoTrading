#!/usr/bin/Rscript

# Proyecto R 


library(binancer)
library(kableExtra)
library(magrittr)
library(dplyr)
library(xts)
library(dygraphs)
library(TTR)
library(telegram.bot)
library(ggplot2)
library(quantmod)

key_1 <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
secret_1 <- "XXXXXXXXXXXXXXXXXXXXXXXXx"


binance_credentials(key = key_1, secret = secret_1)

symbols <- c("BNBUSDT", "STGUSDT", "ATOMUSDT")
sub_symbols <- c("BNB", "STG", "ATOM")
increase <- 0.05 #Aumento de 5%
run <- TRUE

interval <- "1h"
limit <- 100 #100 horas

## MACD
nFast <- 12 
nSlow <- 26 
nSig <- 9

## RSI
n_rsi <- 14

#Uso de Telegram Bot
token <- "XXXXXXXXXXXXXXXXXXXXXXXX"
bot <- Bot(token = token)
updates <- bot$getUpdates()

# Se utiliza tryCatch para iniciar chat con el bot
tryCatch(
  chat_id <- updates[[1L]]$from_chat_id()
)

updater <- Updater(token = token)


#Criterio de Venta
#Devuelve vector con TRUE si vende y FALSE si no 
cripsell <- function(flag_buy, price){
  sell <- FALSE
  
  time_diff <- Sys.time() - flag_buy$time
  #Vende si precio aumenta un 5% respecto al precio de compra
  #Vende si precio cae por debajo del 2% respecto al precio de compra
  #Vende si el tiempo de trade supera los 30 min
  if(price >= (flag_buy$price + increase*flag_buy$price) | 
     price <= (flag_buy$price - 0.02*flag_buy$price)
     | time_diff > 30){
    
    sell <- TRUE
    
  }
  
  return(sell)
  
}


#Estrategia de Compra

# strategy: Indica 0 si comprar y 1 si no comprar)
cripbuy <- function(){
  
  buy <- data.frame(symbols = symbols,
                    sub_symbols = sub_symbols,
                    strategy = double(length(symbols)), 
                    price = double(length(symbols))
  )
  
  for (symbol in symbols) {
    #Obteniendo datos
    candles <- binance_klines(symbol = symbol, interval = interval, limit = limit)
    
    #Seleccionando datos 
    candles %>%
      select(open, high, low, close) %>%
      xts(order.by = candles$open_time) -> candles.xts
    
    
    
    #Calculando indicadores
    macd <- MACD(candles.xts$close, nFast = nFast, nSlow = nSlow, nSig = nSig, 
                 maType = "SMA", percent = FALSE)
    
    
    # Estrategia: Si hay sobrecompra y la tendencia es a la alza -> Compra (1) 
    strategy <- ifelse ((macd$signal < macd$macd), 1, 0)
    strategy[is.na(strategy)] <-0
    
    if (strategy[length(strategy)]==1) {
      buy[buy$symbols==symbol,3] <- 1
      
      sub_symbol <- buy[buy$symbols==symbol,2]
      
      price <- binance_coins_prices(unit = "USDT") %>% 
        filter(symbol == sub_symbol)
      
      buy[buy$symbols==symbol,4] <- price$usd[1]
    }
    
  }
  
  return(buy)
}


#Funcion Trading d
#Comprar o No Comprar y almacena un csv con el registro de operaciones
trading <- function(){
  bot$sendMessage(chat_id = chat_id, text = "Inicio")
  
  n <- length(symbols)
  
  #Variable que va a indicar si hay una compra en proceso
  flag_buy <- data.frame(buy = ifelse (double(n)==0,FALSE,TRUE), 
                         time = double(n),
                         price = double(n))
  
  
  # Variable que indica los registros de operacines 
  
  register <- data.frame(symbols=character(0),
                         Tiempo_Compra=double(0),
                         Tiempo_Venta=double(0),
                         Precio_Compra=double(0),
                         Precio_Venta=double(0),
                         Ganancia=double(0)) 
  
  while(run){
    
    #Analizando si se compra o no 
    df_buy <- cripbuy()
    
    #Realizar alerta de Comprar o No comprar
    for (i in 1:n) {
      if(flag_buy$buy[i]){
        
        if((cripsell(flag_buy[i,], df_buy$price[i]))){
          #Alerta Venta
          text <- paste(substr(Sys.time(),1,19),"Venta de ",symbols[i]," con una ganancia de:",
                        df_buy$price[i]-flag_buy$price[i])
          
          #Enviando alerta a telegram
          bot$sendMessage(chat_id = chat_id, text = text)
          
          #Registros de operacion 
          register[nrow(register) + 1,] <- list(symbols = symbols[i],
                                                Tiempo_Compra=flag_buy$time[i],
                                                Tiempo_Venta=Sys.time(), 
                                                Precio_Compra=flag_buy$price[i],
                                                Precio_Venta=df_buy$price[i],
                                                Ganancia=df_buy$price[i]-flag_buy$price[i])
          
          
          flag_buy$buy[i] <- FALSE
        }
      }else if((df_buy$strategy[i] == 1)){
        buy_time = Sys.time()
        #Alerta Compra
        text <- paste(substr(buy_time,1,19),"Comprando ", symbols[i], 
                      "por la cantidad de ", df_buy$price[i], "USDT")
        
        
        #Enviando alerta a telegram
        bot$sendMessage(chat_id = chat_id, text = text)
        
        flag_buy[i,] <- c(TRUE, buy_time,df_buy$price[i])
      }
      
    }
    
    #Pausa el script 
    Sys.sleep(60)
    
    updates <- bot$getUpdates()
    text_bot <- updates[[length(updates)]]$message$text
    if (text_bot == "/stop") {
      run <- FALSE
    }
  }
  
  register %>% 
    summarise(profit = sum(Ganancia)) -> profit
  
  final_text <- paste("Se ha detenido el proceso de trading a las",
                      Sys.time(), "con una ganancia total de", profit$profit[1], "USDT")
  bot$sendMessage(chat_id = chat_id, text = final_text)
}

#Inicio
start <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("¡Hola %s! Este bot envía información sobre trading de criptomonedas. Para detener el proceso utilice /stop", 
                                 update$message$from$first_name))
  run <<- TRUE
  #Realizando trading
  trading()
}

# Creando comando /start
start_handler <- CommandHandler("start", start)
updater <- updater + start_handler



# Comenzando el bot
updater$start_polling()


