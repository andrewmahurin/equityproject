Title
========================================================
http://www.epa.gov/airquality/carbonmonoxide/
This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
source("getdata.r")
```

```
## Loading required package: Defaults
## Loading required package: xts
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## 
## Loading required package: TTR
## Version 0.4-0 included new data defaults. See ?getSymbols.
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```r
lowgivenhigh(52.4, scha <- setup("scha"))
```

```
## Removing variable 'Date'
## Removing variable 'Open'
## Removing variable 'High'
## Removing variable 'Low'
## Removing variable 'Close'
## Removing variable 'Volume'
## Removing variable 'Adj.Close'
```

```
## [1] 51.57
```


You can also embed plots, for example:


```r
plot(scha$close)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


