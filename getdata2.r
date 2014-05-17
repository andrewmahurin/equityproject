library("quantmod")
#install.packages("Quandl")
#library("Quandl")
#install.packages("gdata")
library("gdata")
#library("Defaults")
#library(dynlm)
#setDefaults(mean, na.rm = TRUE)
#embed (1:10 ,3)[3,1]
Intercept = 1
true = TRUE

##http://stackoverflow.com/questions/1311920/lagging-variables-in-r
#lagmatrix <- function(x,max.lag){embed(c(rep(NA,max.lag),x),max.lag+1)}
#lagmatrix1 <- function(x,max.lag){embed(c(rep(NA,max.lag),lag1(x)),max.lag+1)}
make.lag = function(k) {
  lagk = function (x) c(rep(NA,k), x[1:(length(x)-k)])
}
  
lag1 = make.lag(1)
  #function (x) c(NA, x[1:(length(x)-1)])
lag2 = make.lag(2)
  #function (x) c(NA,NA, x[1:(length(x)-2)])
lag3 = make.lag (3)
lag4 = make.lag(4)
lag5 = make.lag(5)
lag10 = make.lag(10)

lag6 = make.lag(6)
lag21 = make.lag(21)
lag30 = make.lag(30)
  
lag42 = make.lag(42)
  #function (x) c(rep(NA,60), x[1:(length(x)-60)])
#lag60 = function (x) c(rep(NA,60), x[1:(length(x)-60)])
lag63 = make.lag(63)
lag126 = make.lag(126)
lag252 = make.lag(252)
lag304 = make.lag(304)
#lag5 = make.lag(5)
delag = function(x) c(x[2:length(x)], NA)
nolag = function(x) x
mylag = function(x, k =1)  c(rep(NA, k), x[1:(length(x)-k)])

#from stackoverflow: http://stackoverflow.com/questions/3507744/downloading-yahoo-stock-prices-in-r
#load_gold = function(x = gold) {
#  gold = read.csv("~/data/FUTURE_GC1.csv")
#  dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
#}
reverse_commodity = function(dat = gold) {
x = dat
  x$date <- rev(dat$Date)
  x$open <- rev(dat$Open)
  x$high <- rev(dat$High)
  x$low <- rev(dat$Low)
  x$settle <- rev(dat$Settle)
  x$volume <- rev(dat$Volume)
  x$openinterest <- rev(dat$Open.Interest)
  x = remove.vars(x, names=c("Date","Open","High","Low", "Settle", "Volume","Open.Interest"));
  return(x)
}
#gold2 <- Quandl("OFDP/FUTURE_GC2", collapse="monthly")

load_stock = function(symbol = "SPY") {
  URL <- paste("http://ichart.finance.yahoo.com/table.csv?s=",symbol, sep = "")
  dat <- read.csv(URL)
  dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
  return(dat)
}
reverse = function (dat) {
  x = dat
  x$date <- rev(dat$Date)
  x$open <- rev(dat$Open)
  x$high <- rev(dat$High)
  x$low <- rev(dat$Low)
  x$close <- rev(dat$Close)
  x$volume <- rev(dat$Volume)
  x$adj.close <- rev(dat$Adj.Close)
  x = remove.vars(x, names=c("Date","Open","High","Low", "Close", "Volume","Adj.Close"));
  return(x)
}

init = function (symbol = "SPY") {
x1 = load_stock(symbol)
x1 = reverse(x1)
}
max2 = function (x = x1$high, lookback =1) {
 temp = rep(NA,lookback)
  #temp = matrix(nrow = length(x))
 for (i in (1+lookback):length(x)) {
  temp[i] = max(x[(i - lookback):i])
  }
 return (temp)
}

min2 = function (x = x1$low, lookback =1) {
 temp = rep(NA,lookback)
 for (i in (1+lookback):length(x)) {
  temp[i] = min(x[(i - lookback):i])
 }
 return (temp)
}
periodchange =function(k= 5, x = x1){
  ck = mylag(x$close, k)
  highest = (x$close - ck)/ck
}
periodhigh =function(k= 5, x = x1){
  ck = mylag(x$close, k)
  highest = (max2(x$high,lookback =(k-1)) - ck)/ck
}
periodlow =function(k= 5, x = x1){
  ck = mylag(x$close, k)
  lowest = (min2(x$low,lookback =(k-1)) - ck)/ck
}


#cmatrix = (lagmatrix1(x$close, 29))
#  (x$close - cmatrix)/cmatrix


addvariables = function(x = x1) {     #for extra variables
#attach(x)
c1 = lag1(x$close)
c2 = lag2(x$close)
c3 = lag3(x$close)
c4 = lag4(x$close)
c5 = lag5(x$close)
c6 = lag6(x$close)
c21= lag21(x$close)
c30 = lag30(x$close)
c42 = lag42(x$close)
c63 = lag63(x$close)
c126 = lag126(x$close)
c189 = mylag(x$close, 189)
#c252 = lag252(x$close)
laghigh = lag1(x$high)
nrow(c6)
x$dailychange = (x$close - c1)/ c1
x$openchange = (x$open - c1)/ c1
x$intraday = (x$close - x$open)/x$open
x$intradayhigh = (x$high - x$open)/x$open
x$intradaylow = (x$low - x$open)/x$open
x$dailyhigh = (x$high - c1)/ c1
x$dailylow = (x$low - c1)/ c1
x$midpoint = (x$high + x$low)/ 2
x$dailymidpoint = ((x$high + x$low)/2 - c1)/ c1
x$hightohigh = (x$high - lag1(x$high))/ lag1(x$high)
x$lowtolow = (x$low - lag1(x$low))/ lag1(x$low)
x$uptoclose = (x$high - x$close)/ x$close
x$downclose = (x$low - x$close)/ x$close
x$updown1 = (x$high- x$low)/ x$low
x$dailyvol = (x$high- x$low)/ x$close
x$ratio = x$high/x$low
x$multiple = x$volume * x$updown1
x$high2 = max2(x$high,lookback =1)
x$low2 = min2(x$low,lookback =1)
x$dailyhigh2 = (x$high - c2)/ c2
x$dailylow2 = (x$low - c2)/ c2
x$dailyhigh5 = (x$high - c5)/ c5
x$dailylow5 = (x$low - c5)/ c5
x$weeklyhigh = (max2(x$high,lookback =4) - c5)/ c5
x$weeklylow = (min2(x$low,lookback =4) - c5)/ c5
x$weeklyhigh0 = (max2(x$high,lookback =4) - x$close)/ x$close
x$weeklylow0 = (min2(x$low,lookback =4) - x$close)/ x$close
x$monthlyhigh = (max2(x$high,lookback =20) - c21)/ c21
x$monthlylow = (min2(x$low,lookback =20) - c21)/ c21
x$monthlyhigh0 = (max2(x$high,lookback =20) - x$close)/ x$close
x$monthlylow0 = (min2(x$low,lookback =20) - x$close)/ x$close
#x$close
x$twodaychange = (x$close - lag2(x$close))/ lag2(x$close)
x$twodayhigh = (max2(x$high,lookback =1) - c2)/ c2
x$twodaylow = (min2(x$low,lookback =1) - c2)/ c2
x$weeklychange = (x$close - lag5(x$close))/ lag5(x$close)
x$twoweekchange = (x$close - lag10(x$close))/ lag10(x$close)
#x$monthlychange = (x$close - c30)/ c30
x$monthlychange = (x$close - c21)/ c21
x$twomonthchange = (x$close - c42)/ c42
x$threemonthchange = (x$close - c63)/ c63
x$sixmonthchange = (x$close - c126)/ c126
x$ninemonthchange = (x$close - c189)/ c189
#x$annualchange = (x$close - c252)/ c252
#x$twoyearchange = (x$close - lag304(x$close))/ lag304(x$close)
#x$changemat5 = (lagmatrix(x$close, 4) - lag5(x$close))/ lag5(x$close)
#x$dailyforward = delag1(x$dailychange)
return(x)
}
c1 = lag1(x$close)
c2 = lag2(x$close)
c3 = lag3(x$close)
c4 = lag4(x$close)
c5 = lag5(x$close)
c6 = lag6(x$close)
c21= lag21(x$close)
c30 = lag30(x$close)
c42 = lag42(x$close)
c63 = lag63(x$close)
c126 = lag126(x$close)
c189 = mylag(x$close, 189)
addvariables2 = function(x = x1) {     #for extra variables
  #attach(x)

  #c252 = lag252(x$close)
  laghigh = lag1(x$high)
  nrow(c6)
  x$dailychange = (x$close)/ c1
  x$openchange = (x$open)/ c1
  x$intraday = (x$close)/x$open
  x$intradayhigh = (x$high)/x$open
  x$intradaylow = (x$low)/x$open
  x$dailyhigh = (x$high)/ c1
  x$dailylow = (x$low)/ c1
  x$midpoint = (x$high + x$low)/ 2
  x$dailymidpoint = ((x$high + x$low)/2)/ c1
  x$hightohigh = (x$high)/ lag1(x$high)
  x$lowtolow = (x$low)/ lag1(x$low)
  x$uptoclose = (x$high )/ x$close
  x$downtoclose = (x$low )/ x$close
  x$updown1 = (x$high)/ x$low
  x$dailyvol = (x$high)/ x$close
  x$ratio = x$high/x$low
  x$multiple = x$volume * x$updown1
  x$high2 = max2(x$high,lookback =1)
  x$low2 = min2(x$low,lookback =1)
  x$dailyhigh2 = (x$high)/ c2
  x$dailylow2 = (x$low)/ c2
  x$dailyhigh5 = (x$high)/ c5
  x$dailylow5 = (x$low)/ c5
  x$weeklyhigh = (max2(x$high,lookback =4))/ c5
  x$weeklylow = (min2(x$low,lookback =4))/ c5
  x$weeklyhigh0 = (max2(x$high,lookback =4))/ x$close
  x$weeklylow0 = (min2(x$low,lookback =4))/ x$close
  x$monthlyhigh = (max2(x$high,lookback =20))/ c21
  x$monthlylow = (min2(x$low,lookback =20))/ c21
  x$monthlyhigh0 = (max2(x$high,lookback =20))/ x$close
  x$monthlylow0 = (min2(x$low,lookback =20))/ x$close
  #x$close
  x$twodaychange = (x$close)/ lag2(x$close)
  x$twodayhigh = (max2(x$high,lookback =1))/ c2
  x$twodaylow = (min2(x$low,lookback =1))/ c2
  x$weeklychange = (x$close)/ lag5(x$close)
  x$twoweekchange = (x$close)/ lag10(x$close)
  #x$monthlychange = (x$close - c30)/ c30
  x$monthlychange = (x$close)/ c21
  x$twomonthchange = (x$close)/ c42
  x$threemonthchange = (x$close)/ c63
  x$sixmonthchange = (x$close)/ c126
  x$ninemonthchange = (x$close)/ c189
  x$annualchange = (x$close - c252)/ c252
  x$twoyearchange = (x$close - lag304(x$close))/ lag304(x$close)
  #x$changemat5 = (lagmatrix(x$close, 4) - lag5(x$close))/ lag5(x$close)
  #x$dailyforward = delag1(x$dailychange)
  return(x)
}

combinedmodel = function (x = total) {  #verify correlation between two stocks
#  lagx = apply(x, lag1)
#  summary(lagx)
  model1=lm(x$dailyhigh.x ~ 
              lag1(dailychange.x) +lag1(log(close.x)) + log(lag1(volume.x)) + lag1(updown1.x) + lag1(weeklychange.x) + lag1(monthlychange.x)
              + lag1(dailychange.y) +lag1(log(close.y)) + log(lag1(volume.y)) + lag1(updown1.y) + lag1(weeklychange.y) + lag1(monthlychange.y)
#              + lag1(updown1.y)
            + dailychange.y + dailylow.y + dailyhigh.y
            , data= x); print(summary(model1))

  modelx=lm(dailychange.x ~ dailychange.y, data= x); print(summary(modelx))
  modelx=lm(dailyhigh.x ~ lag1(dailychange.x) + (dailychange.y) + dailylow.y + dailyhigh.y + date, data= x); print(summary(modelx))
#  modely=lm(dailychange.y ~ dailychange.x, data= x); print(summary(modely))
#  modelnew = step(model1)
#  print(summary(modelnew))
  return(modelx)
}
#names(total)
showstats = function(x = x1) {
print(paste("mean change", mean(x$dailychange, na.rm=TRUE)))
print(paste("sd of change",sd(x$dailychange, na.rm=TRUE)))
print(sd(x$dailychange, na.rm=TRUE))
print(paste("mean high",mean(x$dailyhigh, na.rm=TRUE)))
print(paste("sd of high",sd(x$dailyhigh, na.rm=TRUE)))
print(paste("mean low",mean(x$dailylow, na.rm=TRUE)))
print(paste("sd of low",sd(x$dailylow, na.rm=TRUE)))
}


sizeup = function(x = spy1) {
mean(log(lag1(x$close)), na.rm= TRUE)
sd(log(lag1(x$close)), na.rm= TRUE)
mean(log(lag1(x$volume)), na.rm= TRUE)
sd(log(lag1(x$volume)), na.rm= TRUE)
}
#predict dailychange based on daily model
Intercept  =1
ref= function (model = dailymodel, x = x1) {
  expected= (model$coefficients [1] * (1) 
             + model$coefficients [2] *  (x$dailychange)  
             + model$coefficients [3] *  log((x$close)) 
             + model$coefficients [4] *  log((x$volume)) 
             + model$coefficients [5] *  (x$weeklychange) 
             + model$coefficients [6] *  (x$monthlychange) )
}

predictbest = function (model = bestup, x = x1) {
  expect =  (  model$coefficients  [1]  *	(1)	+
                 model$coefficients	[2]	*	(x$dailychange)	+
                 model$coefficients	[3]	*	log((x$close))	+
                 model$coefficients	[4]	*	log((x$volume))	+
                 model$coefficients	[5]	*	(x$updown1)	
#               + model$coefficients	[6]	*	(log(x$volume) *	x$updown1)
            )	 
}
weeklyref = function(model = weeklymodel, x = x1) {
  (  model$coefficients[1]  *	(1)	+
       model$coefficients[2]	*	(x$dailychange)	+
       model$coefficients[3]	*	(log(x$close))	+
       model$coefficients[4]	*	(log(x$volume))	+
       model$coefficients[5]	*	(x$weeklychange)	+
       model$coefficients[6]	*	(x$monthlychange)	)
}
weeklyref2 = function(model = weeklymodel, x = x1) {
  (  model$coefficients[1]  *  (1)	+
       model$coefficients[2]	*	(x$weeklychange)  +
       model$coefficients[3]	*	(x$monthlychange)  )
}
monthlyref = function(model = monthlymodel, x = x1) {
  out1 = ( model$coefficients[1]  *	(1)	+
             model$coefficients[2]	*	(x$dailychange)	+
             model$coefficients[3]	*	(log(x$close))	+
             model$coefficients[4]	*	(log(x$volume))	+
             model$coefficients[5]	*	(x$weeklychange)	+
             model$coefficients[6]	*	(x$monthlychange)	)
}
est2simple = function (model = monthlymodel, x = x1) {
(  model$coefficients[1]	*	1 +
     model$coefficients[2]	*	(dailyhigh)	+
     model$coefficients[3]	*	(dailylow)	)
}
est2 = function (model = monthlymodel, ifhigh = 1 , iflow = 0, x = x1){
  (  model$coefficients[1]  *	(1)	+
       model$coefficients[2]	*	ifhigh	+
       model$coefficients[3]	*	iflow	+
       model$coefficients[4]	*	(dailychange)	+
       model$coefficients[5]	*	(log(close))	+
       model$coefficients[6]	*	(log(volume))	+
       model$coefficients[7]	*	(weeklychange)	+
       model$coefficients[8]	*	(monthlychange)	)
}
upref= function (model = dailymodel, x = x1) {
  (  model$coefficients[1]  *  (1)	+
       model$coefficients[2]	*	(dailyhigh > 0) + 
       model$coefficients[3]	*(openchange > 0)  +
  model$coefficients[4]  *	(dailychange)	+
    model$coefficients[5]	*	(log(close))	+
    model$coefficients[6]	*	(log(volume))	+
    model$coefficients[7]	*	(weeklychange)	+
    model$coefficients[8]	*	(monthlychange)	)
}

est3 = function (model = monthlymodel, x = x1){
  (  model$coefficients[1]  *  (1)	+
       model$coefficients[2]	*	dailyhigh	+
       model$coefficients[3]	*	dailylow	+
       model$coefficients[4]	*	(dailychange)	+
       model$coefficients[5]	*	(log(close))	+
       model$coefficients[6]	*	(log(volume))	+
       model$coefficients[7]	*	(weeklychange)	+
       model$coefficients[8]	*	(monthlychange)	)
}
setup = function (sym1 = "SCHB"){
  x1 = init(sym1)
  x1 = addvariables(x1)
}
setup2 = function (sym1 = "SCHB"){
  x1 = init(sym1)
  x1 = addvariables2(x1)
}
gettwo = function (sym1 = "SCHB", sym2 = "FNDA") {
  x1 = init(sym1)
  x2 = init(sym2)
  x1 = addvariables(x1)
  x2 = addvariables(x2)
}

