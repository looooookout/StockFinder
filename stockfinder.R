#library(alphavantager)

#av_api_key("RTEYTSVUTTKWD5V8")


#############################################
#get from yahoo
library(rlist)
library(tidyquant)
library(googlesheets4)
gsurl <- "https://docs.google.com/spreadsheets/d/1NTKj_lzg2H7snC0bbG1_Pf_vNS08-cW8x_x9nrnxN5E"


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
        
        #model.high <- lm(high ~ date, data = stock[stock$date >= (Sys.Date() - 180), ])
        #model.low <- lm(low ~ date, data = stock[stock$date >= (Sys.Date() - 180), ])
        model.interest <- lm(adjusted ~ date, data = stock[stock$date >= (Sys.Date() - 180), ])
        
        #pred.high <- as.data.frame(predict(model.high, interval = "prediction"))
        #pred.low <- as.data.frame(predict(model.low, interval = "prediction"))
        pred.interest <- as.data.frame(predict(model.interest, interval = "prediction"))
        
        stock.sum <- data.frame(
            exchange = index.all[match(stock$symbol[1], index.all$symbol), "exchange"],
            stock.symb = stock$symbol[1],
            company = index.all[match(stock$symbol[1], index.all$symbol), "company"],
            country = index.all[match(stock$symbol[1], index.all$symbol), "country"],
            industry = index.all[match(stock$symbol[1], index.all$symbol), "industry"],
            ipo.year = index.all[match(stock$symbol[1], index.all$symbol), "ipo.year"],
            stock.age = year(tail(stock$date, 1)) - year(head(stock$date, 1)),
            
            company = index.all[match(stock$symbol[1], index.all$symbol), "market.cap"],
            current.price = tail(stock$adjusted,1),
            stock.vel1 = pred.vel[1,1],
            stock.vel2 = pred.vel[1,2],
            stock.vel3 = pred.vel[1,3],
            reachighest = paste(round(as.numeric((stock[stock$adjusted == quantile(stock$adjusted, na.rm = T)[5], ]$date) - (stock[1, ]$date)) / 
                                      as.numeric((tail(stock$date,1)) - (stock$date[1])) * 100, digits = 3), "%", sep = ""),
            monthly.interest.n180 = cal_interestrate(a=head(pred.interest$fit,1), b=last(pred.interest$fit,1), days = 180),
            #delta.range = tail(pred.high$fit,1) - tail(pred.low$fit,1),
            pred.lwr = round(tail(pred.interest$lwr, 1), digits = 3),
            pred.median = round(tail(pred.interest$fit, 1), digits = 3),
            pred.upr = round(tail(pred.interest$upr, 1), digits = 3)
            
        )

        stock.sum$is.interested <- stock.sum$stock.vel1<=stock.sum$stock.vel2 & 
                                   stock.sum$stock.vel2<=stock.sum$stock.vel3 & 
                                   as.numeric(substr(stock.sum$reachighest, 1,6)) > 90

        return(stock.sum)
    }else{return(NA)}
}
stock_summary_new <- function(stock.symbols){
    #request stock data from Yahoo
    stock <- tq_get(stock.symbols)
    if(!is.na(stock)){
        # filet out data before 2020 and error
        stock <- stock[year(stock$date) >= 2020, ]
        stock <- stock[!is.na(stock$adjusted), ]
        # new 2 cols named "MA200", "MA200.trend"
        stock$MA200 <- NA
        stock$MA200.trend <- NA
        # MA200.utl stands for MA 200 uptrand last how many days
        MA200.utl <- 0
        
        # ensure stock has enough data to cal MA200
        if (nrow(stock) >= 230) {
            for(i in c(200:nrow(stock))) {
                # cal MA200
                stock$MA200[i] <- AVERAGE(stock$adjusted[c((i-199):i)])
                # define the MA 200 is go up or down, count how many days the trend continuos go up
                if (is.na(stock$MA200[i-1]) | is.na(stock$MA200[i])) {
                    stock$MA200.trend[i] <- "Undefined"
                }else if (stock$MA200[i] > stock$MA200[i-1]) {
                    stock$MA200.trend[i] <- "UP"
                    MA200.utl <- MA200.utl + 1
                }else if ((stock$MA200[i] < stock$MA200[i-1])) {
                    stock$MA200.trend[i] <- "DOWN"
                    MA200.utl <- 0
                }else if ((stock$MA200[i] = stock$MA200[i-1])) {
                    stock$MA200.trend[i] <- "HOLD"
                    MA200.utl <- 0
        }}}
        
        # enlist data to summary, described as the name
        stock.sum <- data.frame(
            exchange = index.all[match(stock$symbol[1], index.all$symbol), "exchange"],
            stock.symb = stock$symbol[1],
            company = index.all[match(stock$symbol[1], index.all$symbol), "company"],
            country = index.all[match(stock$symbol[1], index.all$symbol), "country"],
            industry = index.all[match(stock$symbol[1], index.all$symbol), "industry"],
            ipo.year = index.all[match(stock$symbol[1], index.all$symbol), "ipo.year"],
            stock.age = year(tail(stock$date, 1)) - year(head(stock$date, 1)),
            market.cap = index.all[match(stock$symbol[1], index.all$symbol), "market.cap"],
            current.price = tail(stock$adjusted,1),
            
            MA50 = AVERAGE(tail(stock$adjusted, 50)),
            MA150 = AVERAGE(tail(stock$adjusted, 150)),
            MA200 = AVERAGE(tail(stock$adjusted, 200)),
            high.52wks = max(tail(stock$adjusted, 52*7), na.rm = T),
            low.52wks = min(tail(stock$adjusted, 52*7), na.rm = T),
            MA50.vol = AVERAGE(tail(stock$volume, 50)),
            
            MA200.uptrend.last = MA200.utl
        )
        
        # determinate if the stock matches with strategy pre-condition, described as follow
        stock.sum$is.interested <- stock.sum$current.price >= stock.sum$MA50 &
                                    stock.sum$current.price >= stock.sum$MA150 &
                                    stock.sum$current.price >= stock.sum$MA200 &
                                    stock.sum$MA50 >= stock.sum$MA150 &
                                    stock.sum$MA50 >= stock.sum$MA200 &
                                    stock.sum$MA200.uptrend.last >= 31 &
                                    stock.sum$current.price >= stock.sum$low.52wks * 1.25 &
                                    stock.sum$current.price <= stock.sum$high.52wks * 0.75 &
                                    stock.sum$current.price > 20 &
                                    stock.sum$market.cap >= 300000000 &
                                    stock.sum$MA50.vol >=50000
                                    
        
        return(stock.sum)
    }else{return(NA)}
}

index.all <- c()
for (exc in tq_exchange_options()){
    tmp = tq_exchange(exc)
    tmp$exchange = exc
    index.all <- rbind(index.all, tmp)
}
index.all <- index.all[!duplicated(index.all$symbol), ]
options(warn=-1)
exchange.sum <- c()
sum.type <- "full"
for (stock.symbol in index.all$symbol) {
    print(paste("getting data: ", stock.symbol, sep = ""))
    tmp = try(stock_summary_new(stock.symbols = stock.symbol))
    if (!is.na(tmp)){
        exchange.sum <- rbind(exchange.sum, tmp)
    }
    print(tail(exchange.sum,1))
}

exchange.sum <- exchange.sum[!duplicated(exchange.sum$stock.symb), ]
#exchange.sum[, c("stock.age", "market.cap", "current.price", "MA50", "MA150", "MA200", 
#                 "high.52wks", "low.52wks", "MA50.vol", "MA200.uptrend.last")] <- as.numeric(
#    exchange.sum[, c("stock.age", "market.cap", "current.price", "MA50", "MA150", "MA200", 
#                     "high.52wks", "low.52wks", "MA50.vol", "MA200.uptrend.last")] )

#publish to google sheet
sheet_rename(gsurl, sheet = 1, new_name = paste("stock.sum", sum.type, as.character(Sys.Date()), sep = "_"))
write_sheet(exchange.sum, ss = gsurl, sheet = paste("stock.sum", sum.type, as.character(Sys.Date()), sep = "_"))


