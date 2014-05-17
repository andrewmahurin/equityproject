install.packages("tseries")
library(tseries)
#http://stackoverflow.com/questions/20872762/is-get-hist-quote-still-returning-data-with-source-yahoo-finance
myghq <- tseries::get.hist.quote
fix(myghq)
save(myghq, file="~/program/myghq.R")
load(file="~/program/myghq.R")
source(file="~/program/getdata.R")
load(file="~/data/equity/SPY1.Rdata")
x = x1
attach(x1)
#x2 = xzoo[complete.cases(xzoo),]
#head(x2)
#x = x2

#plots
plot(lag1(xzoo$hightohigh), xzoo$hightohigh)
par(mfcol=c(3,3))
i = 1
me = apply(x1,2, FUN=function(x) {summary(lm((x)~ lag1(x)))})
i = i+ 1;title(main=names(x1)[i])})
                                                                   "hello world")})
names(x1)[2]
me
names(me)
me2 = sapply(x1,FUN=function(x) 
  for (names in names(x1))
  {plot(lag1(eval(parse(text =name))), eval(parse(text =name))); title(name)}
)
me2
names(x1)
x1$name
k = 1
lagk = make.lag(k)
, x$dailylow, x$dailychange, x$hightohigh, x$lowtolow
model = lm(x$dailyhigh ~lagk(x$lowtolow) + lagk(x$hightohigh)+ lagk(x$weeklyhigh) )
             #lagk(x$dailyhigh^2) 

           #+ lagk(x$lowtolow^2)
           
          # + lagk(x$hightohigh^2)
          # + lagk(x$lowtolow^3) 
          
          #+ lagk(x$weeklyhigh^2)
          )
summary(model)
generalfit(model1)
(model1$coefficients)
eval(parse(text =names(model$coefficients[2]))
(model1$coefficients) * eval(parse(text =names(model$coefficients)))
x4 = eval(parse(text =names(model$coefficients)))
x4 * model1$coefficients
length(x4)
str(x4)
tail(x4)
tail(x1)
x = xzoo
plot(lag1(x$hightohigh), x$dailychange)
plot(lag1(x$hightohigh), x$dailyhigh)
plot(lag1(x$lowtolow), x$dailyhigh)
plot(lag1(x$lowtolow), x$lowtolow) ##
plot(lag1(log(x$hightohigh+1)), x$hightohigh)
par(mfcol=c(2,2))
plot(lag1(x$downclose), x$lowtolow)  ###
plot(lag1(x$downclose), x$hightohigh)  ###
plot(lag1(x$uptoclose), x$hightohigh)###
plot(lag1(x$uptoclose), x$lowtolow)###
plot(lag1(x$downclose), x$dailyhigh)  ###
plot(lag1(x$downclose), x$dailylow)  ###
summary(lm(cbind(x1$lowtohigh, diff1(x$lowtohigh))~ lag1(x$lowtohigh ) +  lag1(diff1(x$lowtohigh))))
summary(lm(x$hightolow~ lag1(x$lowtohigh )))
summary(lm(x$dailychange ~ lag1(x$dailychange) + lag1(diff1(x$dailychange)) + lag1(twodaychange) ))
+ lag1(log(x$close)) 
+ lag1(weeklychange)  
+ lag1(monthlychange)
summdiff(hightolow)
summdiff(lowtohigh)
summdiff(x$dailylow)
summdiff(x$dailychange)
summdiff(x$hightohigh)
summdiff(x$dailyhigh)
summary(lm(x$dailylow~ lag1(x$downclose) ))
summary(lm(x$dailylow~ lag1(x$downclose) +lag2(x$downclose) + lag3(x$downclose) + lag4(x$downclose) + lag5(x$downclose) + lag6(x$downclose) + mylag(x$downclose,7) + mylag(x$downclose,8) + mylag(x$downclose,9) + lag10(x$downclose) + lag1(x$dailychange) + lag1(log(volume))))
summary(lm(x$dailyhigh~ lag1(x$uptoclose) +lag2(x$uptoclose) + lag3(x$uptoclose) + lag4(x$uptoclose) + lag5(x$uptoclose) + lag6(x$uptoclose) + mylag(x$uptoclose,7) + mylag(x$uptoclose,8) + mylag(x$uptoclose,9) + lag10(x$uptoclose)))
summary(lm(x$dailyhigh~ lag1(x$hightolow) +lag2(x$hightolow) + lag3(x$hightolow) + lag4(x$hightolow) + lag5(x$hightolow) + lag6(x$hightolow) + mylag(x$hightolow,7) + mylag(x$hightolow,8) + mylag(x$hightolow,9) + lag10(x$hightolow)))
summary(lm(x$dailyhigh~ lag1(x$lowtohigh) +lag2(x$lowtohigh) + lag3(x$lowtohigh) + lag4(x$lowtohigh) + lag5(x$lowtohigh)
           + lag6(x$lowtohigh) + mylag(x$lowtohigh,7) + mylag(x$lowtohigh,8) + mylag(x$lowtohigh,9) + lag10(x$lowtohigh)))
summary(lm(x$dailyhigh~ lag1(x$lowtohigh) +lag2(x$lowtohigh) + lag3(x$lowtohigh) + lag4(x$lowtohigh) + lag5(x$lowtohigh)
+ lag1(x$hightolow) +lag2(x$hightolow) + lag3(x$hightolow) + lag4(x$hightolow) + lag5(x$hightolow)
))
summary(lm(x$dailyhigh~ lag1(x$dailychange) +lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange)
           ))
summary(lm(x$lowtohigh~ 
             lag1(x$dailylow) 
           +
             lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange) 
           + lag6(x$dailychange) + mylag(x$dailychange,7) + mylag(x$dailychange,8) + mylag(x$dailychange,9) + lag10(x$dailychange) 
            + lag10(lag1(x$dailychange)) +lag10(lag2(x$dailychange)) + lag10(lag3(x$dailychange)) + lag10(lag4(x$dailychange)) + lag10(lag5(x$dailychange)) + lag10(lag6(x$dailychange)) + mylag(x$dailychange,17) + mylag(x$dailychange,18) + mylag(x$dailychange,19) + lag10(lag10(x$dailychange)) 

           ))
summary(lm(x$hightolow~ lag1(x$dailyhigh) +lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange) + lag6(x$dailychange) + mylag(x$dailychange,7) + mylag(x$dailychange,8) + mylag(x$dailychange,9) + lag10(x$dailychange) 
            + lag10(lag1(x$dailychange)) +lag10(lag2(x$dailychange)) + lag10(lag3(x$dailychange)) + lag10(lag4(x$dailychange)) + lag10(lag5(x$dailychange)) + lag10(lag6(x$dailychange)) + mylag(x$dailychange,17) + mylag(x$dailychange,18) + mylag(x$dailychange,19) + lag10(lag10(x$dailychange)) 
))
summary(lm(x$hightolow~ lag1(x$dailychange) +lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange) + lag6(x$dailychange) + mylag(x$dailychange,7) + mylag(x$dailychange,8) + mylag(x$dailychange,9) + lag10(x$dailychange) 
           + lag10(lag1(x$dailychange)) +lag10(lag2(x$dailychange)) + lag10(lag3(x$dailychange)) + lag10(lag4(x$dailychange)) + lag10(lag5(x$dailychange)) + lag10(lag6(x$dailychange)) + mylag(x$dailychange,17) + mylag(x$dailychange,18) + mylag(x$dailychange,19) + lag10(lag10(x$dailychange)) 
))
stats(lm((x$hightolow)~ lag1(x$dailychange) +lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange) + lag6(x$dailychange) + mylag(x$dailychange,7) + mylag(x$dailychange,8) + mylag(x$dailychange,9) + lag10(x$dailychange) 
        + lag10(lag1(x$dailychange)) +lag10(lag2(x$dailychange)) + lag10(lag3(x$dailychange)) + lag10(lag4(x$dailychange)) + lag10(lag5(x$dailychange)) + lag10(lag6(x$dailychange)) + mylag(x$dailychange,17) + mylag(x$dailychange,18) + mylag(x$dailychange,19) + lag10(lag10(x$dailychange)) 
)$residuals)
stats(lowtohigh)
for (k in 1:10) {
  print(summary(lm(delag(periodlow(k),k)~ lag1(x$dailychange) +lag2(x$dailychange) + lag3(x$dailychange) + lag4(x$dailychange) + lag5(x$dailychange) + lag6(x$dailychange) + mylag(x$dailychange,7) + mylag(x$dailychange,8) + mylag(x$dailychange,9) + lag10(x$dailychange) 
             + lag10(lag1(x$dailychange)) +lag10(lag2(x$dailychange)) + lag10(lag3(x$dailychange)) + lag10(lag4(x$dailychange)) + lag10(lag5(x$dailychange)) + lag10(lag6(x$dailychange)) + mylag(x$dailychange,17) + mylag(x$dailychange,18) + mylag(x$dailychange,19) + lag10(lag10(x$dailychange)) 
    +lag1(log(close))
             )))
}
$fitted.values
)
))
+lag5(x$weeklychange) + lag10(x$weeklychange)
hist(x$lowtohigh)
summary(x$hightolow)
hist(x$hightolow, breaks = 40)
hist(x$lowtohigh, breaks = 40)
sd(x$hightolow, na.rm = TRUE)
mean(x$hightolow, na.rm = TRUE)
mean(x$lowtohigh, na.rm = TRUE)
sd(x$lowtohigh, na.rm = TRUE)
sd(x$dailychange, na.rm = TRUE)
sd(x$dailylow, na.rm = TRUE)
sd(x$dailyhigh, na.rm = TRUE)
sd(x$hightohigh, na.rm = TRUE)
sd(x$lowtolow, na.rm = TRUE)
sapply(x1,mean, na.rm = TRUE)
plot((x$downclose~ lag1(x$downclose) ))
+ mylag(x$downclose,7)

sapply(x,mean, na.rm = TRUE)
sapply(x,sd, na.rm = TRUE)

plot(x$dailychange - lag1(x$dailychange)~I(lag1(x$dailychange) - lag2(x$dailychange))) 

summary(model <-lm(x$dailychange - lag1(x$dailychange)~lag1(x$dailychange) 
#                   +    lagk(x$lowtolow)  + 
 #                    lagk(x$hightohigh) +
#+                     lagk(twodaychange) 
#                   + lagk(log(close))
#                   + lagk(log(volume))
#                   + lagk(close) 
#                   + lagk(x$dailyhigh) 
#                   + lagk(x$dailylow)
#                   + lagk(monthlyhigh) 
#                   + lagk(monthlylow)
#                   + lagk(x$weeklychange)  
#                   + lagk(monthlychange)  
#                   + lagk(twomonthchange)
#                   + lagk(threemonthchange)
#                   + lagk(sixmonthchange) 
#                   + lagk(ninemonthchange)
#                   + lagk(annualchange)
#                   + lagk(twoyearchange)
                   , na.action= na.exclude)) #99.98 percent
plot(model$residuals~ lag1(x$dailychange))
plot(x$dailyhigh ~lag1(diff1(x$dailyvol)))
plot(x$dailyvol ~lag1(diff1(x$dailyvol)))
plot(x$dailyvol ~lag1((x$dailyvol)))
for (i in 1:20){

  plot(x$dailyvol ~mylag(x$dailyvol, i))
  abline(x$dailyvol ~mylag(x$dailyvol, i), col = 'green')
  loess(x$dailyvol ~mylag(x$dailyvol, i), col = 'red')
  abline(0,1, col = 'blue')
  
  title(main =i)
}
summary(lm(x$dailyvol ~lag1((x$dailyvol)))) ## 43 percent ###
par( mfcol= c(2,1))
plot(offresid(model <-lm(x$dailyvol ~lag1((x$dailyvol)) + lag1(x$lowtohigh) + lag1(x$hightolow))) )
summary(model)
~ lag1(x$hightolow)
plot(offresid(model))
hist(offresid(model), breaks = 60)
, data = laggedx1)

summary(model <- lm((x$dailyvol) ~lag1((x$dailyvol))+ lag1(diff1(x$dailyvol)) + lag1(x$lowtohigh) + lag1(diff1(x$lowtohigh)) + lag1(x$hightolow) + lag1(x$dailychange))) # 52 percent
summary(lm(diff1(x$dailyvol) ~lag1((x$dailyvol)))) #17 percent
summary(lm(x$dailychange - lag1(x$dailychange)~lag1(x$dailychange) + lag1(x$weeklychange)))
summary(lm(x$weeklychange - lag5(x$weeklychange)~(lag5(x$weeklychange) + lag10(x$weeklychange)))) #55 percent
summary(lm(monthlychange - lag21(monthlychange)~(lag30(monthlychange) + lag42(monthlychange)))) #18 percent
summary(lm(x$dailychange - lag1(x$dailychange)~(lag1(x$dailychange) + lag2(x$dailychange) + lag1(x$dailyhigh) + lag1(x$dailylow) )))
plot(x$dailychange - lag1(x$dailychange)~I(lag1(x$dailychange) - lag2(x$dailychange)))
plot(x$dailychange - lag1(x$dailychange)~(lag1(x$dailychange) + lag2(x$dailychange)))
plot(x$dailychange ~I(lag1(x$dailychange) - lag2(x$dailychange)))
apply(X=x,MARGIN=2,FUN=mean)
?cor
mean(abs(diff1(x$dailychange)), na.rm= TRUE)
hist(diff1(x$dailyhigh), breaks = 40)
plot((lm(diff1(x$lowtolow)~ lag1(x$lowtolow) )$residuals))
plot(diff1(x$lowtolow))
plot(diff2(x$lowtolow))
summary(lm((x$lowtolow)~ lag1(x$lowtolow)))
summary(lm(diff1(x$lowtolow)~ lag1(x$lowtolow))) #46 percent
summary(lm(diff1(x$hightohigh)~ lag1(x$hightohigh))) #46 percent also
summary(lm(diff1(x$hightolow)~ lag1(x$hightolow))) #22.36 percent
summary(lm(diff1(x$lowtohigh)~ lag1(x$lowtolow))) #50 percent
summary(lm(diff1(x$lowtohigh)~ lag1(x$lowtohigh))) #27 percent
summary(lm(diff2(x$lowtolow)~ lag1(x$lowtolow))) #66 percent
summary(lm(diff1(x$lowtolow)~ lag1(diff1(x$lowtolow)))) #17 percent
summary(lm(diff2(x$lowtolow)~ lag1(diff1(x$lowtolow)))) #70.83 percent
summary(lm(diff2(x$lowtolow)~ lag1(diff1(x$lowtolow)) + lag1(x$lowtolow))) #81 percent
summary(lm(diff2(x$lowtolow)~ lag1(diff1(x$lowtolow)) + lag1(x$lowtolow) + lag1(x$dailychange))) #86 percent 
summary(lm(diff3(x$lowtolow)~ lag1(diff2(x$lowtolow)) )) #80 percent
summary(lm(diff3(x$lowtolow)~ lag1(diff2(x$lowtolow))           + lag1(x$lowtolow))) #93.22 percent
summary(lm(diff3(x$lowtolow)~ lag1(diff2(x$lowtolow)) +lag1(diff1(x$lowtolow))  + lag1(x$lowtolow))) #94.2 percent
summary(lm(diff3(x$dailychange)~ lag1(diff2(x$dailychange)) +lag1(diff1(x$dailychange))  + lag1(x$dailychange))) #95.35 percent
summary(lm(x$dailychange~ lag1(diff2(x$dailychange)) +lag1(diff1(x$dailychange))  + lag1(x$dailychange))) 
hist(diff1(x$dailychange), breaks = 40)
plot(diff1(x$dailychange))
#94.2 percent
#46 percent

)
lag1
str(x$lowtolow)
diff1(x$lowtolow)
delag(x$weeklyhigh, 5)
      delag(1:5,2)
##dickey fuller
model = lm(x$dailychange ~ lag(x$dailychange) - 1)
model = lm(formula1)
summary(model)
summary(model1 <- lm(diff1(x$dailychange) ~diff1(offresid(model)) -1 ))
(model1$coefficients[1] - 1 )/   summary(model1)$coefficients[2]
plot(offresid(model)[5500:length(x)])
plot(tail(resid(model), 300) ~ tail(x$dailyhigh,300))
?end(x)
summary(model1)
gdx <- myghq(instrument = "GDX", start = "2004-01-01", end = "2004-12-31")
head(gdx)
tail(gdx)
var.names<-tolower(colnames(gdx))
                   colnames(gdx)<-var.names
reshape(data=gdx, names(gdx), direction="long")
gdx[1,]
#+ lag2(dailychange) + lag1(downclose)