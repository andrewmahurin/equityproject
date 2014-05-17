source("~/program/getdata.r")
sym1 = "SPY"
sym2 = "gld"
#referencforecast()
x1 = setup(sym1)
x2 = setup(sym2)
save(x1, file=paste0("~/data/equity/",sym1,"1.Rdata"))
save(x2, file=paste0("~/data/equity/",sym2,"1.Rdata"))
load(file=paste0("~/data/equity/",sym1,"1.Rdata"))
load(file=paste0("~/data/equity/",sym2,"1.Rdata"))
x = x1
x1$date[(nrow(x1))]<=(Sys.Date()-2)
attach(x1)
#highmodel=lm(dailyhigh ~ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(highmodel))
#lowmodel=lm(dailylow ~ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(lowmodel))
for (var in list(dailychange, dailyhigh,dailylow)) {
  model=lm(cbind(lowtolow) ~ lag1(lowtolow) + lag1(log(close)) + lag1(dailychange) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
  model=lm(cbind(hightohigh) ~ lag1(hightohigh) + lag1(log(close)) + lag1(dailychange) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
  model=lm(cbind(dailychange) ~ lag1(hightohigh) + lag1(log(close)) + lag1(dailychange) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
  model=lm(cbind(hightolow) ~ lag1(hightohigh)  
           + lag1(dailychange) + lag2(dailychange) 
           +lag1(dailyvol)+lag2(dailyvol)+lag3(dailyvol)+lag4(dailyvol)
           , data= x);print(summary(model))
  model=lm(cbind(lowtohigh) ~ lag1(hightohigh) + lag1(log(close)) + lag1(dailychange) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
  +diff1(lag1(dailyvol))
  + lag1(log(close))
  + lag1(log(volume)) 
  + lag3(dailychange) + lag4(dailychange) 
  + lag1(weeklychange)
  #print(summary(dailymodel))  
  + lag1(monthlychange)
  #  print(ref(model)[(nrow(x1)-5):nrow(x1)])
  print(tail(ref(model)[(nrow(x1)-5):nrow(x1)]))
  print(tail(fitted(model)))
  print(tail(var))
}
#x = na.omit(x)
#x1 = na.omit(x)
print(tail(intraday))
model=lm(intraday ~ openchange + lag1(openchange) + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
print(summary(model))
model=lm(intraday ~ lag1(openchange) + lag1(intraday) + lag1(dailychange) + lag1(twodaychange) +lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) +lag1(twoweekchange) + lag1(monthlychange)+ lag1(twomonthchange) + lag1(threemonthchange) + lag1(sixmonth) + lag1(annualchange), data= x, na.action =na.exclude);
print(summary(model))


model
max(rsq[1:21])
model$
  str(model)
#+ lag1(weeklyhigh) + lag1(weeklylow)
model$coefficients
((model$residuals -model$model$intradaylow))
 , na.rm =TRUE)
tail(model$effects)
tail(intradayhigh)
tail(model$model$intradayhigh)
?lm

coef(model)
+ lag1(intradayhigh) * lag1(intradaylow)
+ lag1(log(volume))
+ lag1(twodayhigh) + lag1(twodaylow)
+lag1(twoweekchange)
+ lag1(log(unclass(date)))

+ lag1(intraday)  
#model$coefficients[,1]
#predict(model)
summary(step(model))
model=lm(intradaylow ~ lag1(openchange) + lag1(intraday)  + lag1(twodaychange) + lag1(log(close)) + lag1(date) + lag1(close) + lag1(log(volume)) + lag1(weeklychange) +lag1(twoweekchange) + lag1(monthlychange)+ lag1(twomonthchange) + lag1(threemonthchange) + lag1(sixmonth) + lag1(annualchange) + lag1(ninemonthchange), data= x);
#model=lm(intradaylow ~ lag1(openchange) + lag1(intraday) + lag1(dailychange) + lag1(twodaychange) +lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) +lag1(twoweekchange) + lag1(monthlychange)+ lag1(twomonthchange) + lag1(threemonthchange) + lag1(sixmonth) + lag1(annualchange), data= x);
print(summary(model))
#summary(step(model))
model=lm(intradaylow ~  lag1(openchange) + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
print(summary(model))
summary(intradaylow)
hist(openchange, breaks = 40)
hist
summary(intraday[(openchange>.005)], na.rm = TRUE)
     , breaks = 40, include.lowest=FALSE)
, na.rm = TRUE)
sum
(openchange>.005
 , na.rm = TRUE)
  )
  ])
summary(intraday)
model=lm((monthlylow) ~ lag30(log(close)) * lag30(date) + lag30(close) + lag30(log(volume))  + lag30(weeklychange) + lag30(monthlychange) + lag30(dailyhigh) + lag30(weeklyhigh), data= x);
print(summary(model))
+resid(model0)
mean(log(high)- lag1(log(close)) , na.rm =TRUE)
mean(log(high/low) , na.rm =TRUE)
sd(dailyvol, na.rm = TRUE)
mean(dailyhigh, na.rm = TRUE)
lag1(dailychange) 

tail(x1$date)
plot(log(x$high))
model0 = lm(log(x$close)~ date)
lines(resid(model0))
plot(dailychange)
lines(dailyhigh, col = 'green')
lines(dailyvol, col = 'pink')
?resid
lines(I(1.0002^unclass(date+3000)))
#xt = ts(x1); head(xt)
#xz = zoo(x1); head(xz)
#xt = setup(xt)
#x2 = setup(x2) 
total = merge(x1,x2, by.x = "date", by.y = "date", all = FALSE, sort = TRUE)
xy = total
attach(total)
combinedmodel(total)


sapply(x1, mean, na.rm =TRUE)
sapply(x1, sd, na.rm =TRUE)
#showstats(x2)
summary(x1)
#gold
gold = read.csv("~/data/FUTURE_GC1.csv")
gold$Date <- as.Date(gold$Date, "%Y-%m-%d")
gold = reverse_commodity(gold)


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






#plots 

plot(dailychange ~ date, data= x1)
up=mean(x1$dailyhigh, na.rm=TRUE)
down=mean(x1$dailylow, na.rm=TRUE)
abline(h = up, col = 'blue')
abline(h = 2 * up, col = 'red')
abline(h = down, col = 'blue')
abline(h = 2 *down, col = 'red')
plot(dailyhigh2 ~ date, data= x1)
plot(dailylow2 ~ date, data= x1)
x = spy
hist(x1$dailychange)
hist(x1$hightohigh)
hist(x1$lowtolow)

x= x1

forecasttoday = function (sym =sym1, x = x1){
  #reference
  model = lm(dailychange ~ (dailyhigh > 0) + (dailylow < 0) + (openchange<.004) + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
  model = lm(dailyhigh ~ (dailyhigh > 0) + (dailylow < 0) + (openchange<.004) +lag1(dailychange) + (lag1(log(close)) + lag1(log(volume))) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
  model = lm(dailylow ~ (dailyhigh > 0) + (dailylow < 0) + (openchange > -.004)+ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
  for (mark in 0:3 *.001){
    print(mark)
    model = lm(dailyhigh ~ (dailyhigh > mark) + (dailylow < -mark) + log(volume), data = x); print(summary(model))
    model = lm(dailylow ~ log(lag1(close)) + (dailyhigh > mark) + (dailylow < -mark) + log(volume), data = x); print(summary(model))
  }
  model = lm(twodaychange ~ lag1(dailyhigh > 0) + lag1(dailylow < 0) + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x); print(summary(model))
  model = lm(twodayhigh ~ lag1(dailyhigh > 0) + lag1(dailylow < 0) + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x); print(summary(model))
  model = lm(twodaylow ~ (lag1(dailyhigh > 0) + lag1(dailylow < 0)) * lag2(dailychange) * lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x); print(summary(model))
  
  dailymodel2=lm(twodaychange ~ lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);summary(dailymodel2)
}
forecast = function (sym =sym1, x = x1){
  change = lm(dailychange ~ (openchange) + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(change))
  high = lm(dailyhigh ~ (openchange) +lag1(dailychange) + (lag1(log(close)) + lag1(log(volume))) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(high))
  low = lm(dailylow ~  (openchange)+ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(low))
  return(list(change,high,low))
}
referencemodel = function(k= 2, x = x1){
  #x$periodhigh =
  ck = mylag(x$close, k)
  change = (x$close - ck)/ ck
  highest = (max2(x$high,lookback =(k-1)) - ck)/ck
  lowest = (min2(x$low,lookback =(k-1)) - ck)/ck
  #mylag(x$close, leaddays) 
  model=lm(change ~ mylag(dailychange, k) + mylag(log(close, k)) + mylag(log(volume, k)) + mylag(weeklychange, k) + mylag(monthlychange, k), data= x);print(summary(model))
  highmodel=lm(highest ~ mylag(dailychange, k) + mylag(log(close, k)) + mylag(log(volume, k)) + mylag(weeklychange, k) + mylag(monthlychange, k), data= x); print(summary(highmodel))
  lowmodel=lm(lowest ~ mylag(dailychange, k) + mylag(log(close, k)) + mylag(log(volume, k)) + mylag(weeklychange, k) + mylag(monthlychange, k), data= x); print(summary(lowmodel))
  #return(list(model, highmodel, lowmodel))
}
#straight line

model=lm(log(close) - lag1(log(close))~ date 
         + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange)
         , data= x);print(summary(model))
plot(fitted(model, data = x))
fitted(model)
#log(open) + 
#coremodel = function (x = x1){
#reference point models
dailymodel2=lm(twodaychange ~ lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);summary(dailymodel2)
highmodel2=lm(twodayhigh ~ lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);summary(dailymodel2)
lowmodel2=lm(twodaylow ~ lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);summary(dailymodel2)

highmodel2=lm(dailyhigh2 ~ lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);print(summary(highmodel2))
lowmodel2=lm(dailylow2 ~ lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);
print(summary(lowmodel2))




midmodel= lm(dailymidpoint ~ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(midmodel))
volmodel=lm(dailylow ~ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(volmodel))
volmodel=lm(updown1 ~ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)), data= x); print(summary(volmodel))
weeklymodel=lm(weeklychange ~ lag5(dailychange) +lag5(log(close)) + lag5(log(volume)) + lag5(weeklychange) + lag5(monthlychange), data= x);
weeklyhighmodel=lm(weeklyhigh ~ lag5(dailychange) +lag5(log(close)) + lag5(log(volume)) + lag5(weeklychange) + lag5(monthlychange), data= x);
weeklylowmodel=lm(weeklylow ~ lag5(dailychange) +lag5(log(close)) + lag5(log(volume)) + lag5(weeklychange) + lag5(monthlychange), data= x);

oldweeklymodel=lm(weeklychange ~ lag1(dailychange) +lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
oldweeklyhighmodel=lm(weeklyhigh ~ lag1(dailychange) +lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
oldweeklylowmodel=lm(weeklylow ~ lag1(dailychange) +lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x);
monthlymodel=lm(monthlychange ~ lag30(dailychange) +lag30(log(close)) + lag30(log(volume)) + lag30(weeklychange) + lag30(monthlychange), data= x);summary(monthlymodel)
monthlyhighmodel=lm(monthlyhigh ~ lag30(dailychange) +lag30(log(close)) + lag30(log(volume)) + lag30(weeklychange) + lag30(monthlychange), data= x);summary(monthlyhighmodel)
monthlylowmodel=lm(monthlylow ~ lag30(dailychange) +lag30(log(close)) + lag30(log(volume)) + lag30(weeklychange) + lag30(monthlychange), data= x);summary(monthlylowmodel)

#reduced models
weeklymodel=lm(weeklychange ~  lag5(weeklychange) + lag5(monthlychange), data= x);summary(weeklymodel)
weeklyhighmodel=lm(weeklyhigh ~  lag5(twodaychange) + lag5(log(close)) + lag5(log(volume)) + lag5(weeklychange) + lag5(monthlychange), data= x);summary(weeklyhighmodel)
weeklylowmodel=lm(weeklylow ~  lag5(log(close)) + lag5(log(volume)), data= x);summary(weeklylowmodel)

print(
  summary(weeklymodel))
print(
  summary(weeklyhighmodel))
print(
  summary(weeklylowmodel))
print
(summary(monthlymodel))
print
(summary(monthlyhighmodel))
print(summary(monthlylowmodel))
#probability of a fill
k = .007
summary(fillup <-glm((dailyhigh>k) ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x1, family = binomial(link = "logit")))
filldown=glm((dailylow< -k) ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x1, family = binomial(link = "logit"))
summary(changeup <-glm((dailychange >k) ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x1, family = binomial(link = "logit")))
summary(changedown <-glm((dailychange< -k) ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x1, family = binomial(link = "logit")))
lag2(dailyhigh<.005) +
  lag1(dailylow<.005) +
  twist <-lm(dailyhigh ~ 
               lag1(dailyhigh> -.005) +lag2(dailyhigh> -.005) +
               lag1(dailychange) +
               lag1(log(volume)) + 
               lag1(log(close)) + 
               lag1(weeklychange) + 
               lag1(monthlychange), data= x1)
summary(twist)
#highlow models
dailymodel2=lm(dailychange ~ lag1(dailyhigh > .005) + lag1(dailylow > -.005) + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);summary(dailymodel2)
highmodel2=lm(dailyhigh2 ~ lag1(dailyhigh >.005) + lag1(dailylow > -.005) + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);print(summary(highmodel2))
lowmodel2=lm(dailylow2 ~ lag1(dailyhigh>.005) + lag1(dailylow > -.005)  + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);print(summary(lowmodel2))
highmodel2=lm(dailyhigh ~ lag1(dailyhigh) + lag1(dailylow) + lag1(weeklyhigh0) + lag1(weeklylow0) + lag1(monthlylow) + lag1(updown1), data = x1);summary(highmodel2)
highmodel2=lm(dailyhigh2 ~ lag1(dailyhigh) + lag1(dailylow) + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange)+ lag1(weeklyhigh) + lag1(weeklylow) + lag1(monthlylow) + lag1(updown1), data= x1);print(summary(highmodel2))
highmodel2=lm(dailyhigh2 ~ lag1(dailyhigh) + lag1(dailylow) + lag1(weeklyhigh0) + lag1(weeklylow0) + lag1(monthlyhigh0) + lag1(monthlylow), data = x1);summary(highmodel2)

highmodel3=lm(dailyhigh2 ~ lag1(dailyhigh ) + lag1(dailylow) + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);print(summary(highmodel3))
lowmodel3=lm(dailylow2 ~ lag1(dailyhigh) + lag1(dailylow)  + lag2(dailychange) + lag2(log(close)) + lag2(log(volume)) + lag2(weeklychange) + lag2(monthlychange), data= x);print(summary(lowmodel3))
weeklyhighmodel2=lm(weeklyhigh ~ lag5(dailyhigh) + lag5(dailylow) + lag5(weeklyhigh ) + lag5(weeklylow) + lag5(dailychange) + lag5(log(close)) + lag5(log(volume)) + lag5(weeklychange) + lag5(monthlychange), data= x);print(summary(weeklyhighmodel2))
weeklylowmodel2=lm(weeklylow ~ lag5(dailyhigh) + lag5(dailylow)  + lag5(dailychange) + lag5(log(close)) + lag5(log(volume)) + lag5(weeklychange) + lag5(monthlychange), data= x);print(summary(weeklylowmodel2))

highmodel2=lm(dailyhigh ~ lag1(dailyhigh> .005) + lag1(dailylow > -.005) 
              + lag2(log(close))
              + lag2(dailychange)
              , data = x1);summary(highmodel2) 
lowmodel2=lm(dailylow2 ~ lag1(dailyhigh > .005) + lag1(dailylow < -.005), data = x1);summary(lowmodel2) 
highmodel2=lm(dailyhigh2 ~ lag1(dailyhigh> .00) + lag1(dailylow < -.005), data = x1);summary(highmodel2) 
#     * lag1(dailyhigh>0) * lag1(dailylow>0)
#      + lag1(weeklyhigh0) + lag1(weeklylow0) + lag1(monthlylow) + lag1(updown1)
summary(lag1(x1$dailychange))
summary(lag1(x1$dailylow))

lowmodel5=lm(dailylow5 ~ lag1(dailyhigh) + lag1(dailylow) 
             + lag1(weeklyhigh0) + lag1(weeklylow0) + lag1(monthlylow) + lag1(updown1)
             , data = x1);summary(lowmodel5)

dailysell = function (k) (k* (x1$dailyhigh>k)) + (x1$dailychange * (x1$dailyhigh<=k)) #selling at k , or holding, with value relative to cash
dailybuy = function (k) (k* (x1$dailylow<k)) - (x1$dailychange * (x1$dailylow>=k)) #buying at k discount from previous close, otherwise holding cash while market goes up
dailysellstatusquo = function (k) x1$dailychange * (x1$dailyhigh>k) + x1$dailychange * (x1$dailyhigh<=k) #not selling at k
dailysellregrets = function (k) (k - x1$dailychange) * (x1$dailyhigh>k) + 0 # oppportunity cost of selling at k

dailybuystatusquo = function (k) -x1$dailychange * (x1$dailyhigh>k) - x1$dailychange * (x1$dailyhigh<=k) #not buying at k
dailybuyregrets = function (k) (k + x1$dailychange) * (x1$dailyhigh<k) - 0 # oppportunity cost of buying at k, versus not buying
make.sellmean = function (k) {
  mean(dailysell(k), na.rm = TRUE)
}
sellmean = function (k) mean(dailysell(k), na.rm = TRUE)
sum(x1$dailyhigh>k, na.rm =TRUE)
out = 1:10
for (k in (-10:40) * .001){
  print(mean(dailybuy(-k), na.rm = TRUE))
  #sum(x1$dailyhigh>k, na.rm =TRUE),
  #k)
}
plot(x1$dailyhigh- x1$dailychange~x1$dailychange)
hist(x1$dailyhigh- x1$dailychange)
plot(x1$dailylow~x1$dailychange)
for (i in (5:15) * .001)
  plot(x1$dailychange~x1$dailylow<(-i))
summary(model<- lm(x1$dailychange~x1$dailylow<.01))
out[1]
?print(sellmean(i), i)
optimize(make.sellmean, c(-.015,.015))
sym1
nrow(x1)
#x1$sellprice = dailysell(  lag1(ref(highmodel)))
#x1$sell = dailysell(lag1(ref(weeklyhighmodel)))
#cbind(x1$date, x1$sellprice, x1$dailyhigh)
#par(mfrow = c(1,1))
#plot(x1$sell ~x1$date)
#lines(x1$dailychange ~x1$date)
mean(dailybuy, na.rm= TRUE)
mean(dailysell, na.rm= TRUE)
mean(dailybuystatusquo, na.rm= TRUE)
mean(dailybuyregrets, na.rm= TRUE)
mean(dailysellregrets, na.rm= TRUE)

mean(x1$dailychange, na.rm= TRUE)
summary(fillup); 
summary.glm(filldown)
example(glm)
#demo(glm.vr)
anova(fillup, test = "Chisq")
k
#}
#matrix models
i = 10
length(changematrix)
nrow(changematrix)
matmodel=lm(dailyhigh ~ changematrix * lag1(log(close)) * lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x1);
print(summary(matmodel))
, data = x1)
#dynlm model
simplemodel=dynlm(dailychange ~ lag1(dailychange), data= xz); print(summary(simplemodel))
m2 = dynlm(close~ lag1(close), data= xt); print(summary(m2))
#models = function (x = spy1) {
#volume based models
model=lm(dailyhigh ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)), data= x); print(summary(model))
model=lm(dailylow ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)), data= x); print(summary(model))
model=lm(dailychange ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)), data= x); print(summary(model))

model=lm(hightohigh ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)), data= x); print(summary(model))
model=lm(dailyhigh ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(hightohigh), data= x); print(summary(model))


model=lm(dailyhigh ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1), data= x); print(summary(model))
model=lm(dailylow ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1), data= x); print(summary(model))
model=lm(dailychange ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1), data= x); print(summary(model))
model=lm(updown1 ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1), data= x); print(summary(model))
model=lm(updown1 ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1) + log(lag1(multiple)), data= x); print(summary(model))

model=lm(dailyhigh ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(ratio), data= x); print(summary(model))
#best models
bestvol=lm(updown1 ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1) 
           + lag1(log(volume)*updown1)
           , data= x);print(summary(bestvol))
bestup=lm(dailyhigh ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1) + lag1(log(volume)*updown1), data= x);print(summary(bestup))
bestdown=lm(dailylow ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1) + lag1(log(volume)*updown1), data= x);print(summary(bestdown))
bestdaily=lm(dailychange ~ lag1(dailychange) +log(lag1(close)) + log(lag1(volume)) + lag1(updown1) + lag1(log(volume)*updown1), data= x);print(summary(bestdaily))
#simple models
model=lm(dailyhigh ~ lag1(dailychange), data= x); print(summary(model))
model=lm(dailylow ~ lag1(dailychange), data= x); print(summary(model))
simplemodel=lm(dailychange ~ lag1(dailychange), data= x); print(summary(simplemodel))

model=lm(dailyhigh ~ lag1(dailyhigh), data= x); print(summary(model))
model=lm(dailylow ~ lag1(dailyhigh), data= x); print(summary(model))
model=lm(dailychange ~ lag1(dailyhigh), data= x); print(summary(model))

model=lm(dailyhigh ~ lag1(dailylow), data= x); print(summary(model))
model=lm(dailylow ~ lag1(dailylow), data= x); print(summary(model))
model=lm(dailychange ~ lag1(dailylow), data= x); print(summary(model))

model=lm(dailychange ~ lag1(dailychange) + lag1(dailylow) + lag1(dailyhigh), data= x); print(summary(model))
model=lm(dailychange ~ lag1(dailychange) + lag1(updown1), data= x); print(summary(model))

model=lm(dailychange ~ lag1(dailychange) + log(lag1(close)), data= x); print(summary(model))
#}


plot(x1$dailychange~x1$date)
lines(offset(dailymodel,x1)~x1$date, data = x1, col = 'red')
##more variables
x1$dailyfit = offset(dailymodel, x1)
x1$lowfit= offset(lowmodel, x1)
x1$highfit= offset(highmodel, x1)
lines(x1$dailyfit~x1$date, col = 'blue')
#more variables
x1$bestup = predictbest(bestup, x1)
x1$bestdown = predictbest(bestdown, x1)
x1$bestvol = predictbest(bestvol, x1)
#more variables
fitlow= ref(model = lowmodel, x1 = x1)
fithigh= ref(highmodel, x1 = x1)
fitdaily= ref(dailymodel, x1 = x1)

weeklylow= ref(model = weeklymodel, x1 = x1)
weeklyhigh= ref(weeklyhighmodel, x1 = x1)
weekly= ref(weeklymodel, x1 = x1)


#fitting the model, checking the model
plot(x1$dailychange~x1$date)
points(x1$dailyhigh~x1$date, col = 'purple')
points(x1$dailylow~x1$date, col = 'navy')
lines(lowfit~x1$date, data = x1, col = 'red')
lines(highfit~x1$date, data = x1, col = 'red')
lines(bestup~x1$date, data = x1, col = 'green')
lines(bestdown~x1$date, data = x1, col = 'green')
lines(dailyfit~x1$date, data = x1, col = 'blue')
lines(bestvol~x1$date, data = x1, col = 'green')
plot(x1$weeklychange~x1$date)
points(x1$weeklyhigh~x1$date, col = 'purple')
points(x1$dailylow5~x1$date, col = 'navy')
lines(ref(weeklylowmodel,x1)~x1$date, data = x1, col = 'red')
lines(ref(weeklyhighmodel,x1)~x1$date, data = x1, col = 'green')
lines(ref(weeklymodel,x1)~x1$date, data = x1, col = 'blue')
plot(x1$weeklyhigh~x1$date)
plot(x1$dailylow5~x1$date)

plot(x1$monthlychange~x1$date)
lines(ref(monthlylowmodel,x1)~x1$date, data = x1, col = 'red')
lines(ref(monthlyhighmodel,x1)~x1$date, data = x1, col = 'green')
lines(ref(monthlymodel,x1)~x1$date, data = x1, col = 'blue')
points(x1$monthlyhigh~x1$date, col = 'pink')
points(x1$monthlylow~x1$date, col = 'brown')
plot(x1$dailyhigh~x1$date)
lines(fitted(lowmodel)~x1$date[33:length(x1$date)], data = x1, col = 'red')
lines(fitted(highmodel)~x1$date[33:length(x1$date)], data = x1, col = 'red')
lines(fitted(dailymodel)~x1$date[33:length(x1$date)], data = x1, col = 'blue')
plot(x1$dailylow~x1$date)
lines(fitted(lowmodel)~x1$date[32:length(x1$date)], data = x1, col = 'red')
lines(fitted(highmodel)~x1$date[32:length(x1$date)], data = x1, col = 'red')
lines(fitted(dailymodel)~x1$date[32:length(x1$date)], data = x1, col = 'blue')
plot(x1$weeklychange ~ lag5(x1$weeklyhigh))
plot(x1$weeklychange ~ lag5(x1$dailyhigh))
plot(x1$weeklychange ~ lag5(x1$dailylow))
plot(x1$updown1~x1$date)
lines(fitted(volmodel)~x1$date[4:length(x1$date)], data = x1, col = 'red')
#find the fills
#setup2= function (x1 = x1) {
#x1$expect = fitted(dailymodel)
x1$buyfill = x1$sellfill = rep(NA, 32)
x1$buyfill[32:length(x1$low)] = (fitted(lowmodel) > x1$low[32:length(x1$low)])
x1$sellfill[32:length(x1$low)] = (fitted(highmodel) < x1$high[32:length(x1$low)])
# return (x1)
#}

p1 = predict(lowmodel, interval = "confidence")
p1
plot(predict(lowmodel, interval = "confidence")~x1$date[33:length(x1$date)])

#make the prediction


#diagnostics
length(x1$date[3:length(x1$date)])
length(x1$close)
length(x1$dailyhigh)
length(fitted(dailymodel))
length(fitted(lowmodel))
length(fitted(volmodel))
tail(fitted(dailymodel))
head(fitted(dailymodel))
length(x1$date[30:length(x1$date)])
tail(x1$weeklychange)
#

head(offset(weeklymodel))
tail(offset(weeklymodel))
monthlymodel$coefficients
dailymodel$coefficients
tail(fitted(weeklymodel))
(ref(weeklymodel))[1107:1112]
tail(x1$weeklychange)

tail(cbind(as.character(x1$date),x1$weeklychange),offset(weeklymodel))
tail(fitted(dailymodel))
est2(dailymodel2)[(nrow(x1)-30):nrow(x1)]
tail(fitted(lowmodel2))
ref(dailymodel)[(nrow(x1)-30):nrow(x1)]
tail(fitted(highmodel2))
ref(highmodel2)[(nrow(x1)-30):nrow(x1)]

tail(ref(dailymodel))
(weeklyref(weeklymodel))[(nrow(x1)-10):nrow(x1)]
tail(fitted(weeklymodel))

for (model in list(monthlymodel, monthlyhighmodel,monthlylowmodel)) {
  print((model))
  print(monthlyref(model)[(nrow(x1)-30):nrow(x1)])
  print(tail(fitted(model)))
}
for (model in list(weeklymodel, weeklyhighmodel,weeklylowmodel)) {
  print(summary(model))
  print(anova(model))
  print(weeklyref2(model)[(nrow(x1)-10):nrow(x1)])
  print(tail(predict(model, interval = "confidence")))
}
ifhigh = c((dailyhigh> .005)[2:nrow(x1)], 1)
ifnotlow = c((dailylow> -.005)[2:nrow(x1)], 0)
est2(highmodel2,ifhigh,ifnotlow)[(nrow(x1)-10):nrow(x1)]
est2(lowmodel2,ifhigh,ifnotlow)[(nrow(x1)-10):nrow(x1)]

#more plotting
plot(est2(lowmodel2,ifhigh,ifnotlow))
plot(dailyhigh2)
plot(dailylow2)
plot(dailylow2 - offset(highmodel3))
plot(dailyhigh2 - offset(lowmodel3))
plot(dailyhigh2 ~offset(highmodel3))
plot(dailylow2 ~offset(lowmodel3))
#, na.rm = TRUE)

lines(offset(highmodel2), col = 'green')
lines(offset(lowmodel2), col = 'red')
lines(offset(highmodel3), col = 'green')
lines(offset(lowmodel3), col = 'red')

tail(fitted(lowmodel2))
tail(x1$date)
