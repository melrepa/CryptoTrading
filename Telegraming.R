library(telegram.bot)

token <- "5714075349:AAER6IE28EsAFPAeqroEWcwgTy38RxTu5Pw"

#Inicializar un chat en Telegram con el bot `R_projectDEM_bot`

bot <- Bot(token = token)
updates <- bot$get_updates()
chat_id <- updates[[1L]]$from_chat_id()
chat_id


