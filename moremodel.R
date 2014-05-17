attach(x1)
print(sym1)
mean(diff1(x$dailychange), na.rm = T)
sd(diff1(x$dailychange), na.rm = T)
hist(diff1(x$dailychange))

mean((x$dailychange), na.rm = T)
sd((x$dailychange), na.rm = T)
hist((x$dailychange))


summary(lm(x$dailychange - lag1(x$dailychange)~lag1(x$dailychange) + lag1(x$weeklychange)))
summary(model  <- lm(x$dailychange~lag1(x$dailychange) + lag1(diff1(dailychange))+ lag1(x$weeklychange) + lag1(diff5_1(weeklychange))))
        summary(model)
summary(lm(x$dailychange~ 
             lag1(x$dailychange) +
             lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange) 
           + lag6(x$dailychange) + mylag(x$dailychange,7) + mylag(x$dailychange,8) + mylag(x$dailychange,9) + lag10(x$dailychange) 
           + lag10(lag1(x$dailychange)) +lag10(lag2(x$dailychange)) + lag10(lag3(x$dailychange)) + lag10(lag4(x$dailychange)) + lag10(lag5(x$dailychange)) + lag10(lag6(x$dailychange)) + mylag(x$dailychange,17) + mylag(x$dailychange,18) + mylag(x$dailychange,19) + lag10(lag10(x$dailychange)) 
           
))
lm(x$dailychange ~ lagmatrix(lag1(diff1(dailychange)), 20))
model=lm(intradayhigh~ 
           lag1(openchange) + lag1(twodaychange) + 
           lag1(log(close))
         + lag1(close) 
         + lag1(dailyhigh) + lag1(dailylow)
         + lag1(monthlyhigh) + lag1(monthlylow)
         + lag1(weeklychange)  + lag1(monthlychange) + lag1(threemonthchange) 
         , data= x);
print(summary(model))
modelfit(model)
