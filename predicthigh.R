source("~/program/getdata.r")
fitmodel = function (model = highmodel){
(  model$coefficients[1]	*	1	+
     model$coefficients[2]	*	(openchange)	+
     model$coefficients[3]	*	(twodaychange)	+
     model$coefficients[4]	*	(log(close))	+
     model$coefficients[5]	*	(close)	+
     model$coefficients[6]	*	(dailyhigh)	+
     model$coefficients[7]	*	(dailylow)	+
     model$coefficients[8]	*	(monthlyhigh)	+
     model$coefficients[9]	*	(monthlylow)	+
     model$coefficients[10]	*	(weeklychange)	+
     model$coefficients[11]	*	(monthlychange)	+
     model$coefficients[12]	*	(threemonthchange)	)
}
fitmodel1 = function (model = highmodel){
(  model$coefficients[1]	*	1	+
     model$coefficients[2]	*	(fitlow1)	+
     model$coefficients[3]	*	(fithigh1)	+
     model$coefficients[4]	*	(openchange)	+
     model$coefficients[5]	*	(twodaychange)	+
     model$coefficients[6]	*	(log(close))	+
     model$coefficients[7]	*	(close)	+
     model$coefficients[8]	*	(dailyhigh)	+
     model$coefficients[9]	*	(dailylow)	+
     model$coefficients[10]	*	(monthlyhigh)	+
     model$coefficients[11]	*	(monthlylow)	+
     model$coefficients[12]	*	(weeklychange)	+
     model$coefficients[13]	*	(monthlychange)	+
     model$coefficients[14]	*	(threemonthchange)	)
}
model=lm(intradayhigh~ 
           lag1(openchange) + lag1(twodaychange) + 
           lag1(log(close))
         + lag1(close) 
         + lag1(dailyhigh) + lag1(dailylow)
         + lag1(monthlyhigh) + lag1(monthlylow)
         + lag1(weeklychange)  + lag1(monthlychange) + lag1(threemonthchange) 
         , data= x);
print(summary(model))
fithigh1 = fitmodel(model)
tail(fithigh1)
tail(fitted(model))


model=lm(weeklylow~ 
           lag5(openchange) + lag5(twodaychange) + 
           lag5(log(close))
         + lag5(close) 
         + lag5(dailyhigh) + lag5(dailylow)
         + lag5(monthlyhigh) + lag5(monthlylow)
         + lag5(weeklychange)  + lag5(monthlychange) + lag5(threemonthchange) 
         , data= x);
print(summary(model))
fitlow1 = fitmodel(model)
sd(model$residuals)
sd(intradaylow)
tail(fitlow1)
tail(fitted(model))
hist(tail(dailylow, 60))
tail(intraday)
model2 = (lm(intradaylow ~lag5(fitlow1) + lag1(fithigh1)+ 
             lag2(openchange) + lag2(twodaychange) + 
             lag2(log(close))
           + lag2(close) 
           + lag2(dailyhigh) + lag2(dailylow)
           + lag2(monthlyhigh) + lag2(monthlylow)
           + lag2(weeklychange)  + lag2(monthlychange) + lag2(threemonthchange) 
             , data = x)
)
summary(model2)
model2$coefficients
fitlow2 = fitmodel1(model2)
tail(fitlow2)
tail(fitted(model2))
