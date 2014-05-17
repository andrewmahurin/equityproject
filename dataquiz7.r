
with(fnda, indexplot(dailyhigh5, type='h', id.method='y', id.n=2, 
  labels=rownames(fnda)))
library(relimp, pos=4)
showData(fnda, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
  maxheight=10)
fix(fnda)
densityPlot( ~ close, data=fnda, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ dailychange, data=fnda, bw="SJ", adjust=1, kernel="gaussian")
library(aplpack, pos=4)
stem.leaf(fnda$weeklylow, na.rm=TRUE)
qqPlot(fnda$dailyhigh, dist="norm", id.method="y", id.n=2, 
  labels=rownames(fnda))
with(fnda, Hist(dailylow, scale="frequency", breaks="Sturges", 
  col="darkgray"))
.x <- seq(0, 1, length.out=1000)
plotDistr(.x, dunif(.x, min=0, max=1), cdf=FALSE, xlab="x", ylab="Density", 
  main=paste("Uniform Distribution:  Minimum=0, Maximum=1"))
remove(.x)

