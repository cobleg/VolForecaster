

# Get SPX data

## Objective: Acquire data on the SP&500, calculate realised volatility and explore patterns

library(PerformanceAnalytics)
library(zoo)
# download data and prepare data as a zoo object
getData <- function(ticker, ...){
  getData <- function(s) read.csv(file=paste('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=', ticker, '&outputsize=full&apikey=GKYDTK1KW2X56JNE&datatype=csv',sep=''));
  library(zoo)
  dat1 = getData(ticker);
  dat1$timestamp <- as.Date(dat1$timestamp, format='%Y-%m-%d')
  dat2 <- zoo(dat1$adjusted_close, dat1$timestamp)
  names(dat2) <- ticker
  return(dat2)
}

SPX <- getData("SPX")

# calculate realised volatility
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

spec <- ugarchspec( mean.model=list( armaOrder=c(2,0) ), variance.model = list( model="eGARCH" ), distribution = "std" )
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


# rolling forecast
library(parallel)

cl = makePSOCKcluster(10)
spec = ugarchspec( mean.model=list( armaOrder=c(2,0) ), variance.model = list(model = "eGARCH"), distribution.model = "jsu" )
roll = ugarchroll( spec, r, n.start = 1000, refit.every = 1000, refit.window = "moving",
                   solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE )
show()
stopCluster(cl)
plot( roll )
