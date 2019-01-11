
## Objective: Decompose S&P 500 index data
sampleLength <- 50 # number of days of history to show
IndexData <- ts(SPX, frequency = 5)
plot( tail(IndexData, sampleLength), type="l", ylab = "Index", xlab="Days", main = "S&P 500" )
WeeklyMAPrice <- filter( IndexData, filter=rep(1/5,5) )
lines( tail(WeeklyMAPrice, sampleLength), col="red" )
MonthlyMAPrice <- filter( IndexData, filter=rep(1/25,25) )
lines( tail( MonthlyMAPrice, sampleLength ), col="blue" )
