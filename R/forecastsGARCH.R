

## Objective: create GARCH models and generate forecasts
source("getData.R")

# Get S&P 500 data
SPX <- getData("SPX")

# calculate returns
r <- diff( log( SPX ), 1)

# inspect data via charts
plot(r)
plot(SPX)

# check for missing values
sum(is.na(r))

# check for autocorrelation in returns data
library(stats)
acf( r, lag = 300, na.action = na.pass )

# calculate realised volatility
realisedVol <- ( rollapply( as.xts( r ), width = 20, FUN=sd.annualized ) )
names( realisedVol ) <- 'RealisedVolatility'
plot(realisedVol)
acf( realisedVol, lag = 300, na.action = na.pass )

# check for unit root in realised volatility
realisedVol[ is.na( realisedVol ) ] <- 0
tseries::adf.test( realisedVol )  # Null hypothesis: non-stationary
tseries::kpss.test( realisedVol )  # Null hypothesis: stationary

# create Kernel density plot
d.realisedVol <- density( realisedVol )
plot(d.realisedVol, type = 'l', lwd = 1)

## estimate GARCH model
library(rugarch)

# find the best fitting ARFIMA model for returns
ARFIMA.fit = autoarfima(data = r, ar.max = 2, ma.max = 2, criterion = "AIC", method = "full")
show(head(ARFIMA.fit$rank.matrix)) # SHOW THE SPECIFICATION OF THE BEST FITTING MODEL
headings <- colnames(ARFIMA.fit$rank.matrix)

# get the specification of the best fitting model
arLength <- length(headings[grepl("^ar", headings)]) - 1
maLength <- length(headings[grepl("^ma", headings)])
firstRow <- ARFIMA.fit$rank.matrix[1, ]

#myList <- headings[ 1:(arLength + maLength) ]
myRow <- firstRow[ 1:(arLength + maLength) ]

# if the sum of the AR indicators is zero, then remove the AR part of the vector
myRow2 <- myRow
if(sum(myRow[1:arLength])==0){
  myRow2 <- myRow[-c(1:arLength)]
} 

# if the sum of the MA indicators is zero, then remove the MA part of the vector
if(sum(myRow2[grepl('^ma', names(myRow2))])==0){
  myRow2 <- myRow2[-which(c(grepl('^ma', names(myRow2))))]
} 

## now construct a list object consisting of fixed AR and/or MA components
myList <- as.list(unname(myRow2)) 
names(myList) <- names(myRow2)
# remove elements not equal to 0
myList <- myList[myList == 0]

# check length of AR and MA components
arLength <- sum(grepl('^ar', names(myRow2)))
maLength <- sum(grepl('^ma', names(myRow2)))

spec <- ugarchspec( mean.model=list( armaOrder=c(arLength,maLength) ), variance.model = list( model="eGARCH" ), distribution = "jsu" )

# if myList is not empty then define fixed compoents in the GARCH model specification
if(length(myList) != 0){
  setfixed(spec) <- myList
}

fit <- ugarchfit( spec, r )

show(fit)
plot(fit,which="all") # diagnostics

# forecast (fixed sample period)
bootp = ugarchboot( fit, method = c("Partial", "Full")[1], n.ahead = 20, n.bootpred = 20 )
show(bootp)
plot(bootp)
annualisedVolatility <- xts( fit@fit$sigma, order.by = index( realisedVol ) )
names( annualisedVolatility ) <- 'AnnualisedVolatility'

dat <- merge( realisedVol, annualisedVolatility )
plot( tail( index(dat), 20), tail( dat$RealisedVol, 20) , type = 'l', xlab = 'Date', ylab = 'Annualised Volatility', 
      main = 'SPX Realised Volatility', ylim = c( min( tail( dat, 20) ) , max( c( tail( dat, 20) ) ) ) )
lines( tail( index(dat), 20), tail( dat$AnnualisedVolatility, 20 ), col='blue' )


dateSeries <- seq(as.Date(colnames(bootp@forc@forecast$seriesFor) ), length.out = 20, by = 1)
forecast <- xts( bootp@forc@forecast$seriesFor * 100, order.by = dateSeries)
plot(forecast, xlab = 'Date' )

# rolling forecast
library(parallel)

cl = makePSOCKcluster(10)
spec = ugarchspec( mean.model=list( armaOrder=c(2,0) ), variance.model = list(model = "eGARCH"), distribution.model = "jsu" )
roll = ugarchroll( spec, r, n.start = 1000, refit.every = 1000, refit.window = "moving",
                   solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE )
show()
stopCluster(cl)
plot( roll )

