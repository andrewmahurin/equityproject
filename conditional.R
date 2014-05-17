source ("~/program/getdata.r")
source("~/program/equityproject/timefit.R")
load(file=paste0("~/data/equity/",sym1,"1.Rdata"))
attach(x1)
k =1
# l = k
lowerlimit = -.005
formula = conditional1  <- (periodchange(k) ~
                lagk(twodaychange) 
              + lagk(log(close))
              + lagk(log(volume))
              + lagk(close) 
              + lagk(dailyhigh) 
              + lagk(dailylow)
              + lagk(monthlyhigh) 
              + lagk(monthlylow)
              + lagk(weeklychange)  
              + lagk(monthlychange)  
              + lagk(twomonthchange)
              + lagk(threemonthchange)
              + lagk(sixmonthchange) 
              + lagk(ninemonthchange)
              + lagk(annualchange)
              + lagk(twoyearchange)
              + (periodlow(k) > lowerlimit)
)
trials = 20
rsq = rep(NA, trials)
#(as.character(conditional1))[[3]]
estimate = matrix (NA, trials,length(all.vars(conditional1)) +1)
stderror = matrix (NA, trials,length(all.vars(conditional1)) +1)
tvalue = matrix (NA, trials,length(all.vars(conditional1)) +1)
pvalue = matrix (NA, trials,length(all.vars(conditional1)) +1)
sigma= rep (NA, trials)
sds = rep (NA, trials)
resid  =rep (NA, trials)
fit2 = fit1  = rep(NA, trials)
scaled = rep(NA, trials)

start =  1
end = trials
for (k in seq(start,end,1)) {
lagk = make.lag(k)
model2 = lm(conditional1)
#print(summary(model2))

#tail(timefit(model2),1) + model$coefficients[18]
#ifnotlow = periodlow(1)> -.005
#length(ifnotlow)
#nrow(x1)
rsq[k] = summary(model2)$r.squared
sigma[k] = summary(model2)$sigma
sds [k] = sd(periodchange(k), na.rm = TRUE)
#resid[k] = summary(model2)$resid
#summary(model2)$
estimate [k,] = (summary(model2)$coefficients[,1])
stderror [k,] = (summary(model2)$coefficients[,2])
tvalue[k,] = (summary(model2)$coefficients[,3])
pvalue[k,] = (summary(model2)$coefficients[,4])

fit1[k] = tail(timefit.yes(model2, k),1)
#  tail(generalfit(model2),1)
scaled[k] = fit1[k]/ k
print(c(k, fit1[k], scaled[k]), rsq[k])
}
#    fitcheck = timefit(model2)
#fitcheck = generalfit(model2)
plot(tail(timefit.yes(model2, k),100))
lines(tail(periodhigh(k),100-k))
#tail(model2$coefficients[18] * c((periodlow(1)> -.005)[2:nrow(x1)], 1))
#summary(model)
formula = conditional1
plotforecasts()
print(lowerlimit)
