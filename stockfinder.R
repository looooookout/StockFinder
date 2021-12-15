#library(alphavantager)

#av_api_key("RTEYTSVUTTKWD5V8")


#############################################
#get from yahoo
library(rlist)
library(tidyquant)
library(googlesheets4)
cal_interestrate <- function(a,b,days){
    ((10^((log10((b/a))/days)))^30 - 1) * 100
}

stock_summary <- function(stock.symbols){
        
    stock <- tq_get(stock.symbols)
    if(!is.na(stock)){
        stock <- stock[year(stock$date) >= 2013, ]
        #monthlist <- unique(substr(stock$date, 1,7))
        #stock.sum <- data.frame(matrix(nrow = length(monthlist), ncol = 2))
        #colnames(stock.sum) <- c("timeframe", "price.avg")
        #stock.sum$timeframe <- monthlist
        #stock.sum$company <- index.all[match(stock$symbol[1], index.all$symbol), "company"][1]
        #stock.sum$sxchange <- index.all[match(stock$symbol[1], index.all$symbol), "exchange"][1]
        quant.stockdays <- round(nrow(stock)/3)
        
        pred.vel <- data.frame(matrix(ncol = 3))
        for(i in c(1:3)){
            if((i * quant.stockdays + 1) < nrow(stock)){
                numofdate = i * quant.stockdays + 1
            }else{numofdate = (nrow(stock)-1)}
            model.phase <- lm(adjusted ~ date, data = stock[(stock$date <= stock$date[numofdate + 1]) & 
                                                                (stock$date > stock$date[(i-1) * quant.stockdays + 1]), ])
            pred.df <- as.data.frame(predict(model.phase, interval = "prediction"))
            pred.vel[1, i] <- (last(pred.df$fit,1) - head(pred.df$fit,1)) / quant.stockdays
        }
        
        model.high <- lm(high ~ date, data = stock[stock$date >= (Sys.Date() - 180), ])
        model.low <- lm(low ~ date, data = stock[stock$date >= (Sys.Date() - 180), ])
        model.interest <- lm(adjusted ~ date, data = stock[stock$date >= (Sys.Date() - 180), ])
        
        pred.high <- as.data.frame(predict(model.high, interval = "prediction"))
        pred.low <- as.data.frame(predict(model.low, interval = "prediction"))
        pred.interest <- as.data.frame(predict(model.interest, interval = "prediction"))
        
        stock.sum <- data.frame(
            exchange = index.all[match(stock$symbol[1], index.all$symbol), "exchange"],
            stock.symb = stock$symbol[1],
            company = index.all[match(stock$symbol[1], index.all$symbol), "company"],
            current.price = tail(stock$adjusted,1),
            stock.vel1 = pred.vel[1,1],
            stock.vel2 = pred.vel[1,2],
            stock.vel3 = pred.vel[1,3],
            reachighest = paste(round(as.numeric((stock[stock$adjusted == quantile(stock$adjusted, na.rm = T)[5], ]$date) - (stock[1, ]$date)) / 
                                      as.numeric((tail(stock$date,1)) - (stock$date[1])) * 100, digits = 3), "%", sep = ""),
            monthly.interest.n180 = cal_interestrate(a=head(pred.interest$fit,1), b=last(pred.interest$fit,1), days = 180),
            delta.range = tail(pred.high$fit,1) - tail(pred.low$fit,1),
            pred.lwr = round(tail(pred.high$lwr, 1), digits = 3),
            pred.median = ((tail(pred.high$lwr, 1) + tail(pred.high$upr, 1))/2),
            pred.upr = round(tail(pred.low$upr, 1), digits = 3)
        )
        
        stock.sum$is.interested <- stock.sum$stock.vel1<=stock.sum$stock.vel2 & 
                                 stock.sum$stock.vel2<=stock.sum$stock.vel3 & 
                                 as.numeric(substr(stock.sum$reachighest, 1,6)) > 90
        
        return(stock.sum)
    }else{return(NA)}
}

index.all <- c()
for (exc in tq_exchange_options()){
    tmp = tq_exchange(exc)
    tmp$exchange = exc
    index.all <- rbind(index.all, tmp)
}
options(warn=-1)
exchange.sum <- c()
sum.type <- "full"
for (stock.symbol in index.all$symbol) {
    print(paste("getting data: ", stock.symbol, sep = ""))
    tmp = try(stock_summary(stock.symbols = stock.symbol))
    if (!is.na(tmp)){
        exchange.sum <- rbind(exchange.sum, tmp)
    }
    print(tail(exchange.sum,1))
}

gsurl <- "https://docs.google.com/spreadsheets/d/1Qazdmqde-ZfoEZBx7XIQveqfDJtKzjKU/edit#gid=169173649"

write_sheet(exchange.sum, ss = gsurl, sheet = paste("stock.sum", sum.type, as.character(Sys.Date()), sep = "_"))
sheet_rename(gsurl, sheet = 1, new_name = paste("stock.sum", sum.type, as.character(Sys.Date()), sep = "_"))



