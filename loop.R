source("~/program/equityproject/getdata.r")
source("~/program/equityproject/timefit.R")
sym1 = "SPY"
x1 = setup(sym1)
load(file=paste0("~/data/equity/",sym1,"1.Rdata"))
save(x1,file=paste0("~/data/equity/",sym1,"1.Rdata"))
x=x1
attach(x1)
laggedx1 = rbind(NA, x1[1:(nrow(x1)-1),])
tail(date)
xzoo = convert_zoo(x1)
#detach(x1)
#attach(xzoo)
#time based model
formula = formula1  <- (periodhigh(k) ~
    lagk(lowtolow)  + 
    lagk(hightohigh) +
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
  )
formula = bigform  <- (periodhigh(k) ~
#                         lagk(dailyhigh - lag1(dailyhigh)) +
                         lagk(lowtolow)  + 
                        +  lagk(hightohigh)
                        + lagk(weeklychange)
                         + lagk(weeklyhigh)
#                        + lagk(weeklylow)
                        +  lagk(twodaychange) 
                        + lagk(log(close))
#                        + lagk(log(volume))
                        + lagk(close) 
                        + lagk(dailyhigh) 
                        + lagk(dailylow)
                        + lagk(monthlyhigh) 
                        + lagk(monthlylow)
                        + lagk(weeklychange)  
                        + lagk(monthlychange)  
#                        + lagk(twomonthchange)
#                        + lagk(threemonthchange)
#                        + lagk(sixmonthchange) 
#                        + lagk(ninemonthchange)
#                        + lagk(annualchange)
#                        + lagk(twoyearchange)
                        )
print(summary(lm(bigform)))
#extractvalues = function(formula1){
#return variables
k = 1
trials = 20
rsq = rep(NA, trials)
#(as.character(formula1))[[3]]
estimate = matrix (NA, trials,length(all.vars(formula)) +1)
stderror = matrix (NA, trials,length(all.vars(formula)) +1)
tvalue = matrix (NA, trials,length(all.vars(formula)) +1)
pvalue = matrix (NA, trials,length(all.vars(formula)) +1)
sigma= rep (NA, trials)
sds = rep (NA, trials)
resid  =rep (NA, trials)
fit2 = fit1  = rep(NA, trials)
scaled = rep(NA, trials)

            #100,length(all.vars(formula)) +1)
#end =  0

#for (i in 1:5) {

#loop
start =  1
  end = trials
  for (k in seq(start,end,1)) {
    lagk = make.lag(k)
    model=lm(bigform, data= x1);
  drop1(model)
    print(summary(model))
  print(k)
    rsq[k] = summary(model)$r.squared
    sigma[k] = summary(model)$sigma
    sds [k] = sd(periodchange(k), na.rm = TRUE)
    #resid[k] = summary(model)$resid
      #summary(model)$
    estimate [k,] = (summary(model)$coefficients[,1])
    stderror [k,] = (summary(model)$coefficients[,2])
    tvalue[k,] = (summary(model)$coefficients[,3])
    pvalue[k,] = (summary(model)$coefficients[,4])
#    fit1[k] = tail(timefit(model), 1)
    fit1[k] = tail(generalfit(model),1)
    scaled[k] = fit1[k]/ k
#    fitcheck = timefit(model)
    fitcheck = generalfit(model)
print(c(k, fit1[k]), rsq[k])
    #plot(fitcheck)
    #lines(fitted(model), col = k)
#    title(main = paste("k", k))
#    plot(c(tail(fitcheck, 200+k)))
#    title(main = paste("k =" ,k))
#    lines(c(tail(offset(model), 200)), col = 'red')
#    lines(tail(periodhigh(k), 200), col = "green")
#    lines(tail(periodchange(k), 200, col = "black"))
#    lines(tail(periodlow(k), 200, col = "red"))

  }
#return(list(rsq, sigma, sds, estimate, stderror, tvalue, pvalue, fit1, scaled, fitcheck))
#}
#list(rsq, sigma, sds, estimate, stderror, tvalue, pvalue, fit1, scaled, fitcheck) = extractvalues(formula)
#sapply(list(rsq, sigma, sds, estimate, stderror, tvalue, pvalue, fit1, scaled, fitcheck), summary)
plot(tail(fit1[k]))
#, fit1[k]
plot(c(tail(fitcheck, 100+k)))
title(main = paste("k =" ,k))
lines(c(tail(offset(model), 100)), col = 'red')
lines(tail(periodhigh(k), 100))

plot(fitcheck)
title(main = paste("k =" ,k))
lines(offset(model), col = 'red')
lines(periodhigh(k))
#  offset(model))), col = 'red')
periodhigh(k)
  fit1[1:k]
scaled
par(mfrow =c(2,1))
  plot(rsq[1:k], ylab = "Percentage of variation explained", xlab = "length of holding period k")
  title (main = list(paste("Model to explain", as.character(formula)[[2]])), sym1)
plot (sds, ylab = "total s.d and model sigma", xlab = "length of holding period k")
lines (sigma, col = "blue")
lines(rsq[1:k], col = 'red')
#fit1[1:k]+sigma[1:k]
#sigma
plotforecasts()
plotvariables()
if (as.character(formula)[[2]] == "periodchange(k)") changefit = fit1
if (as.character(formula)[[2]] == "periodhigh(k)") highfit = fit1
if (as.character(formula)[[2]] == "periodlow(k)") lowfit = fit1
tail(changefit)
tail(highfit)
par(mfrow =c(1,1))
plot(changefit[1:k])
plot(highfit[1:k])
plot(lowfit[1:k])
