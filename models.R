source("~/program/getdata.r")
sym1 = "SPY"
#x1 = loadsym(sym1)
load(file=paste0("~/data/equity/",sym1,"1.Rdata"))
x=x1
attach(x1)
tail(date)
#upmodels
dailymodel = lm(dailychange ~ (dailyhigh > 0) + (openchange > 0) + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
highmodel = lm(dailyhigh ~ (dailyhigh > 0)  + (openchange >0) +lag1(dailychange) + (lag1(log(close)) + lag1(log(volume))) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
lowmodel = lm(dailylow ~ (dailyhigh > 0)  + (openchange > 0)+ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
ifopenup = c((x$openchange> .00)[2:nrow(x1)], 1)
ifhigh = c((x$dailyhigh> .00)[2:nrow(x1)], 1)
for (model in list(dailymodel, highmodel,lowmodel)) {
  print(summary(model))
  #  print(ref(model)[(nrow(x1)-5):nrow(x1)])
  print(tail(upref(model, ifopenup)[(nrow(x1)-5):nrow(x1)]))
  print(tail(fitted(model)))
}
tail(conditionalfit2(model, append.yes(dailyhigh>0.005,1),append.yes(openchange>0, 1)))
#downmodels
model = lm(dailychange ~ (dailyhigh > 0) + (dailylow < 0) + (openchange > 0) + lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
model = lm(dailyhigh ~ (dailyhigh > 0) + (dailylow < 0) + (openchange >0) +lag1(dailychange) + (lag1(log(close)) + lag1(log(volume))) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))
model = lm(dailylow ~ (dailyhigh > 0) + (dailylow < 0) + (openchange > 0)+ lag1(dailychange) + lag1(log(close)) + lag1(log(volume)) + lag1(weeklychange) + lag1(monthlychange), data= x); print(summary(model))