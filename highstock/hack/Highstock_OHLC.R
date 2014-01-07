##
## OHLSV graph
## - Need to fix geometry issues
ohlcvPlot <- highstockOHLCV <- function(name='a stock', data=data, type='candlestick', radius = 3, title = name, subtitle = NULL)
{
    rChart <- Highstock$new()
    
    nrows <- nrow(data)
    data  <- na.omit(data) # remove remaining observations with NA's
    
    if (nrows != nrow(data))
        warning("Observations with NA has been removed")  

    rChart$yAxis(title='OHLC', height=180, linewidth=2)
    rChart$yAxis(title='Volume', top = 250, height=50, offset=0, linewidth=2, replace=F)
    
    rChart$series(
        data = toJSONArray2(data[c('date', 'Open', 'High', 'Low', 'Close')], json = F, names = F),
        type = type,
        name = name,
        marker = list(radius = radius))
    
    rChart$series(
        data = toJSONArray2(data[c('date', 'Volume')], json = F, names = F, replace = F),
        type = 'column',
        name = 'Volume',
        yAxis =1,
        marker = list(radius = radius))
    
    rChart$legend(enabled = F)
    
    ## title
    rChart$title(text = title, replace = T)
    
    ## subtitle
    rChart$subtitle(text = subtitle, replace = T)
    
    return(rChart$copy())
}

##
## OHLC graph
ohlcPlot <- highstockOHLC <- function(name='a stock', data=data, type='candlestick', radius = 3, title = name, subtitle = NULL)
{
    rChart <- Highstock$new()
    
    nrows <- nrow(data)
    data  <- na.omit(data) # remove remaining observations with NA's
    
    if (nrows != nrow(data))
        warning("Observations with NA has been removed")  
    
    rChart$series(
        data = toJSONArray2(data[c('date', 'Open', 'High', 'Low', 'Close')], json = F, names = F),
        type = type,
        name = name,
        marker = list(radius = radius))
    
    rChart$legend(enabled = F)
    
    ## title
    rChart$title(text = title, replace = T)
    
    ## subtitle
    rChart$subtitle(text = subtitle, replace = T)
    
    return(rChart$copy())
}


