library("quantmod")
library("zoo")
library(tseries)
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
lagmatrix <- function(x,max.lag){embed(c(rep(NA,max.lag),x),max.lag+1)}

lagmatrix1 <- function(x,max.lag = 10){embed(c(rep(NA,max.lag),lag1(x)),max.lag+1)}
make.lag = function(k) {
  lagk = function (x) c(rep(NA,k), x[1:(length(x)-k)])
}
lagk = function (x) c(rep(NA,k), x[1:(length(x)-k)])
#make.lag = function(k)  {
#  lagk = function(x) lag(x,-k)
#}
lag1 = make.lag(1)
  #function (x) c(NA, x[1:(length(x)-1)])
lag2 = make.lag(2)
  #function (x) c(NA,NA, x[1:(length(x)-2)])
lag3 = make.lag (3)
lag4 = make.lag(4)
lag5 = make.lag(5)
lag6 = make.lag(6)
lag7 = make.lag(7)
lag8 = make.lag(8)
lag9 = make.lag(9)
lag10 = make.lag(10)
lag11 = make.lag(11)
lag12 = make.lag(12)
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
#delag1 = function(x) c(x[2:length(x)], NA)
delag = function(x,k) c(x[(1+k):length(x)], rep(NA,k))
nolag = function(x) x
mylag = function(x, k =1)  c(rep(NA, k), x[1:(length(x)-k)])
#mylag = function (x, k =1) lag(x,-k)
diff0= function(x) x
diff1 = function(x) {x -lag1(x)}
diff2 = function(x) {diff1(diff1(x))}
diff3 = function(x) {diff1(diff2(x))}
diff5_1 = function(x) {x -lag5(x)}
#{(x -(lag1(x))}
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
loadsym = function(symbol = "SPY") {
  temp =  (load(file=paste0("~/data/equity/",symbol,"1.Rdata")))
           return(temp)
}
convert_zoo = function (x = x1) {
  B <- x
  B$date <- NULL
#  C <- zoo(as.matrix(B))
  z <- zoo(as.matrix(B), order.by=x$date)
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
max3 = function (x = x1$high, lookback =4) {
  #temp = rep(NA,lookback)
  #temp = matrix(nrow = length(x))
  temp = (x$high)
  for (i in 1: 4){
    print(i)
    temp = apply(merge(temp, lag(x$high, -i)), 1, max,na.rm = TRUE)
  #  )
  #    temp = max(temp, lag(x, -i))
  #(temp)
  #head(merge(x$high, lag(x$high)))
  #tail(merge(x$high, lag(x$high)))
  #  for (i in (1+lookback):length(x)) {
  #    temp[i] = max(x[(i - lookback):i])
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
min3 = function (x = x1$low){
  temp = rep(NA, length(x))
  lagx = lag1(x)
  for (i in 1:length(x)) {
    temp[i] =   min(x, lagx, na.rm=TRUE)
  }
return (temp)
}

max3 = function (x = x1$high){
  temp = rep(NA, length(x))
  lagx = lag1(x)
  for (i in 1:length(x)) {
    temp[i] = max(x, lagx)
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
#c189 = mylag(x$close, 189)
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
x$uptoclose = (x$close - x$high)/ x$close
x$downtoclose = x$downclose  = (x$close - x$low)/ x$close
x$hightolow = (x$low - lag1(x$high))/ lag1(x$high)  #
x$lowtohigh = (x$high - lag1(x$low))/ lag1(x$low)  #
x$hightolow1 = (x$low - lag2(x$high))/ lag2(x$high)  #
x$lowtohigh1 = (x$high - lag2(x$low))/ lag2(x$low)  #
x$hightolow2 <- (min3(x$low) - lag2(x$high))/ lag2(x$high)  #
x$lowtohigh2 = (max3(x$high) - lag2(x$low))/ lag2(x$low)  #
#x$uptoclose = (x$high - x$close)/ x$close
#x$downtoclose = (x$low - x$close)/ x$close
#x$hightolow = (lag1(x$high) - x$low)/ x$close  #usually a positive number 
#x$lowtohigh = (lag1(x$low) - x$high)/ x$close  #usually a negative number

x$updown1 = (x$high- x$low)/ x$low
x$dailyvol = (x$high- x$low)/ x$close
x$ratio = x$high/x$low
x$multiple = x$volume * x$updown1
#x$high2 = max2(x$high,lookback =1)
#x$low2 = min2(x$low,lookback =1)
x$dailyhigh2 = (x$high - c2)/ c2
x$dailylow2 = (x$low - c2)/ c2
x$dailyhigh5 = (x$high - c5)/ c5
x$dailylow5 = (x$low - c5)/ c5
x$high5 = max2(x$high,lookback =4)
x$low5 = min2(x$low,lookback =4)
x$high21  = max2(x$high,lookback =20)
x$low21 = min2(x$low,lookback =20)
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
#x$twodayhigh = (max2(x$high,lookback =1) - c2)/ c2
#x$twodaylow = (min2(x$low,lookback =1) - c2)/ c2
x$weeklychange = (x$close - lag5(x$close))/ lag5(x$close)
x$twoweekchange = (x$close - lag10(x$close))/ lag10(x$close)
#x$monthlychange = (x$close - c30)/ c30
x$monthlychange = (x$close - c21)/ c21
x$twomonthchange = (x$close - c42)/ c42
x$threemonthchange = (x$close - c63)/ c63
x$sixmonthchange = (x$close - c126)/ c126
#x$ninemonthchange = (x$close - c189)/ c189
#x$annualchange = (x$close - c252)/ c252
#x$twoyearchange = (x$close - lag304(x$close))/ lag304(x$close)
#x$changemat5 = (lagmatrix(x$close, 4) - lag5(x$close))/ lag5(x$close)
#x$dailyforward = delag1(x$dailychange)
return(x)
}
combinedmodel = function (xy = total) {  #verify correlation between two stocks
#  lagx = apply(x, lag1)
#  summary(lagx)
  model1=lm(x$dailyhigh.x ~ 
              lag1(dailychange.x) +lag1(log(close.x)) + log(lag1(volume.x)) + lag1(updown1.x) + lag1(weeklychange.x) + lag1(monthlychange.x)
              + lag1(dailychange.y) +lag1(log(close.y)) + log(lag1(volume.y)) + lag1(updown1.y) + lag1(weeklychange.y) + lag1(monthlychange.y)
#              + lag1(updown1.y)
            + dailychange.y + dailylow.y + dailyhigh.y
            , data= xy); print(summary(model1))

modelx=lm(dailychange.x ~ dailychange.y, data= xy); print(summary(modelx))
modelweek=lm(weeklychange.x ~ weeklychange.y, data= xy); print(summary(modelweek))
modelmonth=lm(monthlychange.x ~ monthlychange.y, data= xy); print(summary(modelmonth))
modelyear=lm(annualchange.x ~ annualchange.y, data= xy); print(summary(modelyear))
  modelx=lm(dailyhigh.x ~ lag1(dailychange.x) + (dailychange.y), data= xy); print(summary(modelx))
#+ dailylow.y + dailyhigh.y + date
#  modely=lm(dailychange.y ~ dailychange.x, data= xy); print(summary(modely))
#  modelnew = step(model1)
#  print(summary(modelnew))
print(paste(sym1,"vs", sym2))
diffmodel=lm(dailychange.x - dailychange.y ~ lag1(dailychange.x -dailychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(weeklychange.x - weeklychange.y ~ lag5(weeklychange.x -weeklychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(monthlychange.x - monthlychange.y ~ lag21(monthlychange.x -monthlychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(twomonthchange.x - twomonthchange.y ~ lag42(twomonthchange.x -twomonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(threemonthchange.x - threemonthchange.y ~ lag63(threemonthchange.x -threemonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(sixmonthchange.x - sixmonthchange.y ~ lag126(sixmonthchange.x -sixmonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(annualchange.x - annualchange.y ~ lag252(annualchange.x -annualchange.y), data= xy); print(summary(diffmodel))
#twomonthchange
diffmodel=lm(dailychange.x - dailychange.y ~ lag1(dailychange.x -dailychange.y) + lag1(dailychange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(weeklychange.x - weeklychange.y ~ lag5(weeklychange.x -weeklychange.y) + lag5(weeklychange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(monthlychange.x - monthlychange.y ~ lag21(monthlychange.x -monthlychange.y) + lag21(monthlychange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(twomonthchange.x - twomonthchange.y ~ lag42(twomonthchange.x -twomonthchange.y) + lag42(twomonthchange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(threemonthchange.x - threemonthchange.y ~ lag63(threemonthchange.x -threemonthchange.y) + lag63(threemonthchange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(sixmonthchange.x - sixmonthchange.y ~ lag126(sixmonthchange.x -sixmonthchange.y) + lag126(sixmonthchange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(annualchange.x - annualchange.y ~ lag252(annualchange.x -annualchange.y) + lag252(annualchange.x), data= xy); print(summary(diffmodel))

diffmodel=lm(dailychange.x  ~ lag1(dailychange.x -dailychange.y) + lag1(dailychange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(weeklychange.x  ~ lag5(weeklychange.x -weeklychange.y) + lag5(weeklychange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(monthlychange.x  ~ lag21(monthlychange.x -monthlychange.y) + lag21(monthlychange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(twomonthchange.x ~ lag42(twomonthchange.x -twomonthchange.y) + lag42(twomonthchange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(threemonthchange.x ~ lag63(threemonthchange.x -threemonthchange.y) + lag63(threemonthchange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(sixmonthchange.x  ~ lag126(sixmonthchange.x -sixmonthchange.y) + lag126(sixmonthchange.x), data= xy); print(summary(diffmodel))
diffmodel=lm(annualchange.x  ~ lag252(annualchange.x -annualchange.y) + lag252(annualchange.x), data= xy); print(summary(diffmodel))

diffmodel=lm(dailychange.x ~ lag1(dailychange.x -dailychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(weeklychange.x ~ lag5(weeklychange.x -weeklychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(monthlychange.x ~ lag21(monthlychange.x -monthlychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(twomonthchange.x ~ lag42(twomonthchange.x -twomonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(threemonthchange.x ~ lag63(threemonthchange.x -threemonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(sixmonthchange.x ~ lag126(sixmonthchange.x -sixmonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(annualchange.x ~ lag252(annualchange.x -annualchange.y), data= xy); print(summary(diffmodel))

diffmodel=lm(dailychange.x ~ lag1(dailychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(weeklychange.x ~ lag5(weeklychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(monthlychange.x ~ lag21(monthlychange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(twomonthchange.x ~ lag42(twomonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(threemonthchange.x ~ lag63(threemonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(sixmonthchange.x ~ lag126(sixmonthchange.y), data= xy); print(summary(diffmodel))
diffmodel=lm(annualchange.x ~ lag252(annualchange.y) + annualchange.y, data= xy); print(summary(diffmodel))
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
setupzoo = function (sym1 = "SCHB"){
  x1 = init(sym1)
  x1 = convert_zoo(x1)
  x1 = addvariables(x1)
}

gettwo = function (sym1 = "SCHB", sym2 = "FNDA") {
  x1 = init(sym1)
  x2 = init(sym2)
  x1 = addvariables(x1)
  x2 = addvariables(x2)
}

#model = simplemodel
offset = function(model = dailymodel, x = x1) {
  #lag1=nolag
  fit = fitted(model, data = x)
  #  length(fit)
  #  nrow(x)
  off = length(x$date) -length(fit)
  x$fit = c(rep(NA, off), fit)
}
offresid = function(model = dailymodel, x = x1) {
   resid1 = residuals(model, data = x)
   off = length(x$date) -length(resid1)
  x$redid2 = c(rep(NA, off), resid1)
}
assume.yes = function(k  =1){
  rep(1,k)
}
assume.no = function(k  =1){
  rep(0,k)
}
append.yes = function(x = (x1$dailyhigh> .005), k  =1){
  c(x[k:length(x)], rep(1,k))
} 
generalfit = function(model = model1){
  total = 0
  Intercept  = 1
  #total = model$coefficients[i] * Intercept
  lag1 = lagk = nolag
  for (i in 1:length(model$coefficients)){
    print(names(model$coefficients[i]))
    total = total + model$coefficients[i] * eval(parse(text =names(model$coefficients[i])))
  }
  return (total)
}
conditionalfit = function(model = model1, passedvariable){
  total = 0
  Intercept  = 1
  total = model$coefficients[1] * Intercept
  total = total + model$coefficients[2] * passedvariable
  lagk = nolag
  for (i in 3:length(length(model$coefficients))){
    total = total + model$coefficients[i] * eval(parse(text =names(model$coefficients[i])))
  }
  return (total)
}

conditionalfit2 = function(model = model1, passedvariable1, passedvariable2){
  total = 0
  Intercept  = 1
  total = total + model$coefficients[1] * Intercept
  print(length(total))
  total = total + model$coefficients[2] * passedvariable1
  print(length(total))
  total = total + model$coefficients[3] * passedvariable2
  print(length(total))
  lagk = nolag
  for (i in 4:((length(model$coefficients)))){
    print(i)
    total = total + model$coefficients[i] * eval(parse(text =names(model$coefficients[i])))
    print(length(total))
  }
  return (total)
}
plotforecasts = function (){
  par(mfrow =c(2,1))
  plot (fit1[1:k], ylab = "percent", xlab = "length of holding period k")
  lines(fit1[1:k]+sigma[1:k], col = "green")
  lines(fit1-sigma, col = "red")
  title (main = paste("expected", as.character(formula)[[2]], "during holding period k"), sub = sym1)
  plot (scaled[1:k], ylab = "percent per day", xlab = "length of holding period k")
}
plotvariables = function(){
  par(mfrow =c(4,1))
  j = 1
  for (name in 
       names((summary(model)$coefficients)[,1]))
  {
    plot(estimate[,j], xlab = "length of period k in which low, high, or change is to take place", ylab = name)
    title(main = paste("estimate for coefficient in model for predicting", as.character(formula)[[2]]))
    plot(stderror[,j], xlab = "length of period k", ylab = name)
    title(main = paste("std. error", as.character(formula)[[2]]))
    plot(tvalue[,j], xlab = "length of period k", ylab = name)
    title(main = paste("t value", as.character(formula)[[2]]))
    plot(pvalue[,j], xlab = "length of period k", ylab = name)
    title(main = paste("p value", as.character(formula)[[2]]))
    j = j + 1
  }
}
sell = function(x = x1){
  #stock
  ifsold = (x$high> ask)
  deposit = ifsold * ask
  return(c(ifsold, deposit))
}
buy = function (x = x1) {
  ifbought = (x$low< bid)
  withdrawal = ifsold * bid
  return(c(ifbought, withdrawal))
  
}
findprofits  = function (x= x1){
  dailyholdingprofit = x$close - lag1(x$close)
  holdingcashprofit = 0
  initialcash = 0
  initialstock = 0
  #weekly
    ask = lag1(x$close) * (1 +.0067)
    askcash =   ask * lag1(x$high> ask)
  stocksoldatask = 1 * lag1(x$high> ask)
  #initialstock - 
  #bid
  #profit
  secondarybid = ask * (1/(1 -.0067))
  askcash =   ask * (x$low< bid)  
}
summdiff  = function (var = x$dailychange) {
  print(summary(lm(var ~ lag1(var))))
  print(summary(lm(var ~ lag1(var) + lag1(diff1(var)))))
# print(summary(lm(diff1(var) ~ lag1(var))))
#  print(summary(lm(diff1(var) ~ lag1(var) + lag1(diff1(var)))))
}
#function (x) c(rep(NA,k), x[1:(length(x)-k)])

#http://quanttrader.info/public/testForCoint.html
coint = function(xy = total){
  m <- lm(adj.close.x ~ adj.close.y + 0, data=xy)
  beta <- coef(m)[1]
  summary(m)
  print(paste(sym1,"vs", sym2))
  cat("Assumed hedge ratio is", beta, "\n")
  sprd <- xy$adj.close.x - beta*xy$adj.close.y
  ht <- adf.test(sprd, alternative="stationary", k=0)
  cat("ADF p-value is", ht$p.value, "\n")
  if (ht$p.value < 0.05) {
    cat("The spread is likely mean-reverting\n")
  } else {
    cat("The spread is not mean-reverting.\n")
  }
}
coint2 = function(xy = total){
  m <- lm(dailylow.x ~ lag1(lowtolow.x) + 0, data=xy)
  beta <- coef(m)[1]
  summary(m)
  print(paste(sym1,"vs", sym2))
  cat("Assumed hedge ratio is", beta, "\n")
  sprd <- xy$dailylow.x - beta*lag1(xy$lowtolow.x)
  ht <- adf.test(na.omit(sprd), alternative="stationary", k=0)
  cat("ADF p-value is", ht$p.value, "\n")
  if (ht$p.value < 0.05) {
    cat("The spread is likely mean-reverting\n")
  } else {
    cat("The spread is not mean-reverting.\n")
  }
}

addhalfpercent = function (value = x$close){
  value *1.005
}

increaseby =function (value = x$close, amount = .005){
  value *(1+amount)
}
changexby  = function (amount = .005, x = x1){
  newx = x$close*(1 + amount)
  print (newx)
}
modelfit = function (model = highmodel, x = x1){
  amount  = tail(generalfit(model),1)
  print(tail(amount))
  newx = x$close * (1+ amount)
  print(tail(x$close))
  print(tail(newx))
}

desc  = function (x  = x1$close) {
  print(paste("mean     sd"))
  c(mean(x,na.rm = TRUE), sd (x, na.rm = TRUE))
}
yesterdaysrange  = function (x = x1) {
  print(tail(range <- x$high -x$low,1))
  print(tail(x$dailyvol,1))
  range/x$close
  par(mfcol = c(2,1))
  plot(tail(log(range),25))
  plot(tail(x$dailyvol,25))
  hist(range)
  return(tail(range))
}
predictrange = function (model = volmodel, x = x1){
  amount  = tail(generalfit(model),1)
  print(tail(amount))
  rangex = x$close * (amount)
  print(tail(x$close))
  print(tail(rangex))
}
highgivenlow = function(givenlow = lag1(low), x = x1) {
  givenlow * (1+ tail(x$dailyvol,1))
}
lowgivenhigh = function(givenhigh = lag1(high), x = x1) {
  givenhigh * (1- tail(x$dailyvol,1))
}
forcasthigh = function(x = x1, lowmodel= lowmodel1) {
  newup = generalfit(model = lowmodel)
  newhigh  = (x$close)* (1 + newup)
}
