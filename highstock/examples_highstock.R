#rm(list=ls())

require(rCharts)
require(quantmod)
require(plyr)

# Specify period of time we are interested in
startDate <- as.Date("2010-01-01")
endDate   <- as.Date("2013-12-31")

# Define the tickers we are interested in
tickers   <- c('AAPL', 'MSFT', 'MMM', 'V', 'HD', 'GE')
getSymbols(tickers, src = "google", from = startDate, to = endDate)

getQData <- function(fun)
{
    myfunc <- function(name)
    {
        # Remoe stock name from columns
        dt            <- get(name)
        colnames(dt)  <- gsub(paste(name, ".", sep=""), "", colnames(dt), fixed=T)
        
        # Call quantmod function
        tmp           <- fun(dt)
        
        stock         <- data.frame(t=index(tmp), coredata(tmp)) 
        stock$date    <- as.numeric(as.POSIXct(stock$t, origin="1970-01-01")) * 1000
        stock$stock   <- name
        
        return( stock )        
    }
    
    return( myfunc )
}

stocks  <- do.call(rbind, lapply(tickers, getQData(OHLCV)))

##
## Standard plots

# One stock
aapl    <- subset(stocks, stock=='AAPL')
sPlot(Close ~ date, data = aapl, title='AAPL') # Default is line
sPlot(Close ~ date, data = aapl, title='AAPL', type='column') # Default is line
sPlot(Close ~ date, data = aapl, title='AAPL', type='area') # Default is line
sPlot(Close ~ date, data = aapl, title='AAPL', type='area', navigator=F) # Default is line

# Try switching off some highstock features
s <- sPlot(Close ~ date, data = aapl, title='AAPL', type='area'); s
s$rangeSelector(enabled = FALSE, replace = T); s
s$exporting(enabled = FALSE, replace = T); s
s$navigator(enabled = FALSE); s
s$scrollbar(enabled = FALSE); s
s$params

# Multiple stocks
# The legend is clickable
s <- sPlot(Close ~ date, data = stocks, title = 'A few stocks', group = 'stock') # Default is line
s
s$legend(enabled = T, align = 'left', verticalAlign = 'middle', layout = 'vertical', replace = T)
s

# Try modifying the geometry of the container
options(RCHART_WIDTH = 1200, RCHART_HEIGHT = 900)
s <- sPlot(Close ~ date, data = stocks, title='A few stocks', group='stock') # Default is line
s
s$exporting(sourceWidth = 800, sourceHeight = 600, scale = 2); s

##
## OHLC / Candlestick plots

# One stock only
ohlcvPlot(data = aapl, name='AAPL' )
ohlcvPlot(data = aapl, name='AAPL', type='ohlc')

