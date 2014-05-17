library("quantmod")
#install.packages("Quandl")
#library("Quandl")
#install.packages("gdata")
#library("gdata")
#library("Defaults")
library(dynlm)
library("ggplot2")
#install.packages

#setDefaults(mean, na.rm = TRUE)
#embed (1:10 ,3)[3,1]

#lnew = function (x) embed(x,1)
##http://stackoverflow.com/questions/1311920/lagging-variables-in-r
lagmatrix <- function(x,max.lag){embed(c(rep(NA,max.lag),x),max.lag+1)}
lagmatrix1 <- function(x,max.lag){embed(c(rep(NA,max.lag),lag1(x)),max.lag+1)}
make.lag.df = function(k) {
  lagk = function (x) c(rep(NA,k), x[1:(length(x)-k)])
}
make.lag.ts = function(k) {
  lagk = function (x) lag(x,k)
  #c(rep(NA,k), x[1:(length(x)-k)])
}
make.lag= make.lag.ts
lag1 = make.lag(1)
lag2 = make.lag(2)
lag3 = make.lag (3)
lag4 = make.lag(4)
lag5 = make.lag(5)
lag6 = make.lag(6)
lag30 = make.lag(30)
lag60 = make.lag(60)

#delag1 = function(x) c(x[2:length(x)], NA)
nolag = function(x) x

# mylag = function(x, k =1)  c(rep(NA, k), x[1:(length(x)-k)])
mylag = function(x, k=1)  lag(x, -k)  #time series version

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
  test = read.zoo(URL)
  dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
  return(dat)
}
initzoo = function(symbol = "SPY"){
  #dat = getSymbols(symbol, from)
  dat = 
    getSymbols("SPY",  from="1970-01-01", auto.assign=FALSE)
  #head(getSymbols("SCHB",  from="1970-01-01", auto.assign=TRUE))
  #.getSymbols[1]
  #dat = removeSymbols(dat)
  #dat
  
  #rename
  #x$date <- dat$Date)
  dat$open <- Op(dat)
  dat$high <- Hi(dat)
  dat$low <- Lo(dat)
  dat$close <- Cl(dat)
  dat$volume <- Vo(dat)
  dat$adj <- Ad(dat)
  return(dat)
}

#functions
reverse = function (dat) {
#reverse
x = dat
#x = matrix(NA,nrow(dat),ncol(dat))
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
  #temp1 = rep(NA,lookback)
 #temp = matrix(nrow = length(x))
 temp = rep(NA,lookback)
# length(x)
 for (i in (1+lookback):length(x)) {
  temp[i] = min(x[(i - lookback):i])
 }
 return (temp)
}
periodhigh =function(k= 5, x = x1){
  ck = mylag(x$close, k)
  highest = (max2(x$high,lookback =(k-1)) - ck)/ck
}
periodlow =function(k= 5, x = x1){
  ck = mylag(x$close, k)
  lowest = (max2(x$low,lookback =(k-1)) - ck)/ck
}

setup = function(x = x1) {     #for extra variables
#attach(x)
c1 = lag1(x$close)
c2 = lag2(x$close)
c3 = lag3(x$close)
c4 = lag4(x$close)
c5 = lag5(x$close)
c6 = lag6(x$close)
c30 = lag30(x$close)
laghigh = lag1(x$high)
nrow(c6)
x$dailychange = (x$close - c1)/ c1
x$dailyhigh = (x$high - c1)/ c1
x$ratiodown = (x$low - c1)/ c1
x$hightohigh = (x$high - lag1(x$high))/ lag1(x$high)
x$lowtolow = (x$low - lag1(x$low))/ lag1(x$low)
x$upclose = (x$high - x$close)/ x$close
x$downclose = (x$low - x$close)/ x$close
x$updown = (x$high- x$low)/ x$low
x$ratio = x$high/x$low
x$multiple = x$volume * x$updown
x$high2 = max2(x$high,lookback =1)
x$low2 = min2(x$low,lookback =1)
x$dailyhigh2 = (x$high2 - c2)/ c2
x$ratiodown2 = (x$low2 - c2)/ c2
x$weeklyhigh = (max2(x$high,lookback =4) - c5)/ c5
x$ratiodown5 = (min2(x$low,lookback =4) - c5)/ c5
x$monthlyhigh = (max2(x$high,lookback =29) - c30)/ c30
x$monthlylow = (min2(x$low,lookback =29) - c30)/ c30
x$change2 = (x$close - lag2(x$close))/ lag2(x$close) 
x$weeklychange = (x$close - lag5(x$close))/ lag5(x$close)
x$monthlychange = (x$close - c30)/ c30
#x$changemat5 = (lagmatrix(x$close, 4) - lag5(x$close))/ lag5(x$close)
#x$dailyforward = delag1(x$dailychange)
return(x)
}
setupzoo = function(x = xz){
  x$change = diff(x$close)
  return(x)
}
showstats = function(x = x1) {
print(mean(x$dailychange, na.rm=TRUE))
print(sd(x$dailychange, na.rm=TRUE))
print(mean(x$dailyhigh, na.rm=TRUE))
print(sd(x$dailyhigh, na.rm=TRUE))
print(mean(x$ratiodown, na.rm=TRUE))
print(sd(x$ratiodown, na.rm=TRUE))
#mean(x, na.rm=true)
#sd(x, na.rm=true)
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
  expected= (model$coefficients [1] * (Intercept) 
             + model$coefficients [2] *  (x$dailychange)  
             + model$coefficients [3] *  log((x$close)) 
             + model$coefficients [4] *  log((x$volume)) 
             + model$coefficients [5] *  (x$weeklychange) 
             + model$coefficients [6] *  (x$monthlychange) )
}

predictbest = function (model = bestup, x = x1) {
  expect =  (  model$coefficients  [1]  *	(Intercept)	+
                 model$coefficients	[2]	*	(x$dailychange)	+
                 model$coefficients	[3]	*	log((x$close))	+
                 model$coefficients	[4]	*	log((x$volume))	+
                 model$coefficients	[5]	*	(x$updown)	+
                 model$coefficients	[6]	*	(log(x$volume) *	x$updown))	
}
weeklyref = function(model = weeklymodel, x = x1) {
  (  model$coefficients[1]  *	(Intercept)	+
       model$coefficients[2]	*	(dailychange)	+
       model$coefficients[3]	*	(log(close))	+
       model$coefficients[4]	*	(log(volume))	+
       model$coefficients[5]	*	(weeklychange)	+
       model$coefficients[6]	*	(monthlychange)	)
}
monthlyref = function(model = monthlymodel, x = x1) {
  out1 = ( model$coefficients[1]  *	(Intercept)	+
             model$coefficients[2]	*	(dailychange)	+
             model$coefficients[3]	*	(log(close))	+
             model$coefficients[4]	*	(log(volume))	+
             model$coefficients[5]	*	(weeklychange)	+
             model$coefficients[6]	*	(monthlychange)	)
}

#compare2 = function(

#main
sym1 = "Schx"
sym2 = "SCHB"

#x1 = init(sym1)
#xdate = x1$date

#xz = zoo(x1,xdate); 
xz1 =initzoo(sym1)
#xz = zoo(x1$close)
names(xz1)
xz1 = setupzoo(xz1)
xz2 = setup(xz1)
x= xz2
head(xz2)


save(x1, file="~/data/x1.Rdata")

x = xz2
attach(xz2)
xt = ts(x1); head(xt)
xz = zoo(x1); head(xz)
xt = setup(xt)
x2 = setup(x2) 
showstats(x1)
#showstats(x2)
summary(x1)
#gold
  gold = read.csv("~/data/FUTURE_GC1.csv")
  gold$Date <- as.Date(gold$Date, "%Y-%m-%d")
 gold = reverse_commodity(gold)
total = merge(x1,x2, by.x = "date", by.y = "date", all = FALSE, sort = TRUE)

head(total)
tail(total)
print(paste(".x is ",sym1))
print(paste(".y is ",sym2))
x = total
#attach(x)
changematrix = matrix (NA, nrow(x1), 30)
for (i in 1:30) {
  changematrix[,i] = lag1((x$close - mylag(x$close,i))/mylag(x$close,i))
}

