require("data.table");require("quantmod")
# function to calculate 
findBestDay = function(dat)
{
  subYr <- unique(.indexyear(dat))
  res <- lapply(as.list(subYr),function(x)
  {
    YEAR <- dat[.indexyear(dat) == x]
    FIRST <- 1         # index of the first trading day
    LAST <- nrow(YEAR) # index of the last trading day
    # combinations of what day to buy & sell
    DAYS <- expand.grid((FIRST:LAST),(FIRST:LAST))
    # BUY DAY SHOULD BE LOWER THAN SELL DAY
    DAYS <- subset(DAYS,DAYS[,1] < DAYS[,2])
    assign("DAYS",DAYS,envir = .GlobalEnv)
    tmp <- lapply(as.list(1:nrow(DAYS)), function(ii){
      # subsets range
      sub <- YEAR[DAYS[ii,1]:DAYS[ii,2],]
      BOT <- as.numeric(Op(sub)[1])
      WORST <- min(Lo(sub))
      BEST <-  max(Hi(sub))
      CLOSE <- as.numeric(last(Cl(sub)))
      OPCL <-as.numeric(last(Op(sub))) # last Open
      PTS1<- BEST-BOT
      PTS2<- WORST-BOT
      PTS3<- OPCL-BOT
      PTS4<- CLOSE-BOT
      dt<- as.data.frame(cbind(unique(format(index(sub),"%Y")),BOT,BEST,WORST,OPCL,CLOSE, DAYS[ii,1], DAYS[ii,2],PTS1,PTS2,PTS3,PTS4))
      colnames(dt)<-c("YEAR","BOT","MAX","MIN","L.OP","L.CL","BuyIdx","SellIdx","BOT2BEST","BOT2WORST","BOT2LOP","BOT2LCL")
      dt
    })
    tm <- rbindlist(tmp)
    tm
  })
  
  res <- rbindlist(res)
  write.csv(res,"SeasonalDump.csv")
  
# COMBINE BY YEAR
ByEar<- lapply(as.list(1:nrow(DAYS)),function(ii){
    YY <- subset(res,res$BuyIdx == DAYS[ii,1] & res$SellIdx == DAYS[ii,2])
    YY <- YY[,9:ncol(YY)]
    YY <- as.data.frame(sapply(YY,function(x) as.numeric(as.character(x))))
    WIN1 <- round(sum(YY$BOT2LOP >0)/nrow(YY),2)
    WIN2 <- round(sum(YY$BOT2LCL >0)/nrow(YY),2)
    YY <- round(colSums(YY),2)
    YY <- cbind(as.data.frame(DAYS[ii,1]),as.data.frame(DAYS[ii,2]),t(YY),WIN1,WIN2)
    colnames(YY) <- c("BUY","SELL","BOT2BEST","BOT2WORST","BOT2LOP","BOT2LCL","WinOp","WinCl")
    YY
  })
  ByEar <- rbindlist(ByEar)
  ByEar
}

ticker <- "DIA"
dat <- getSymbols(ticker,from="1970-01-01",auto.assign = FALSE)
# Subset December only
dat <- dat[.indexmon(dat)==11]
RES <- findBestDay(dat)
