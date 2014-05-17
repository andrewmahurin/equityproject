timefit = function (model = model1){
(  model$coefficients[1]	*	1	+
     model$coefficients[2]	*	(twodaychange)	+
     model$coefficients[3]	*	(log(close))	+
     model$coefficients[4]	*	(log(volume))	+
     model$coefficients[5]	*	(close)	+
     model$coefficients[6]	*	(dailyhigh)	+
     model$coefficients[7]	*	(dailylow)	+
     model$coefficients[8]	*	(monthlyhigh)	+
     model$coefficients[9]	*	(monthlylow)	+
     model$coefficients[10]	*	(weeklychange)	+
     model$coefficients[11]	*	(monthlychange)	+
     model$coefficients[12]	*	(twomonthchange)	+
     model$coefficients[13]	*	(threemonthchange)	+
     model$coefficients[14]	*	(sixmonthchange)	+
     model$coefficients[15]	*	(ninemonthchange)	+
     model$coefficients[16]	*	(annualchange)	+
     model$coefficients[17]	*	(twoyearchange)	)
}
timefit.yes = function (model = model1, wait = 1){
  (  model$coefficients[1]  *	1	+
       model$coefficients[2]	*	(twodaychange)	+
       model$coefficients[3]	*	(log(close))	+
       model$coefficients[4]	*	(log(volume))	+
       model$coefficients[5]	*	(close)	+
       model$coefficients[6]	*	(dailyhigh)	+
       model$coefficients[7]	*	(dailylow)	+
       model$coefficients[8]	*	(monthlyhigh)	+
       model$coefficients[9]	*	(monthlylow)	+
       model$coefficients[10]	*	(weeklychange)	+
       model$coefficients[11]	*	(monthlychange)	+
       model$coefficients[12]	*	(twomonthchange)	+
       model$coefficients[13]	*	(threemonthchange)	+
       model$coefficients[14]	*	(sixmonthchange)	+
       model$coefficients[15]	*	(ninemonthchange)	+
       model$coefficients[16]	*	(annualchange)	+
       model$coefficients[17]	*	(twoyearchange)	
     + 
       model$coefficients[18] * c((periodlow(k)> -.01)[2:nrow(x1)], 1)
     )
}
scaled  = function (x = fit1){
  for (i in 1: length(x))
  { x [i]= x[i]/i}
}
