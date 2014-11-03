data.annualStockPrices <- function() {
  ticker <- data[ data$ticker != '', ]$ticker
  d.list <- lapply( c( ticker ), data.annualClosePriceFromTicker )
  prices <- ldply( d.list, data.frame )
  data.addSP500( prices )
}

data.annualClosePriceFromTicker <- function( ticker ) {
  d <- data.getPriceData( ticker )
  data.closePriceDataFrame( ticker, d )
}

data.getPriceData <- function( ticker ) {
  d <- try( Quandl( data.nyse( ticker ), collapse = 'annual' ), silent = TRUE )
  if ( class( d ) == 'try-error' ) {
    d <- try( Quandl( data.nasdaq( ticker ), collapse = 'annual' ) )
  }
  if ( class( d ) == 'try-error' ) {
    cat( paste0( 'Quandl call failed with ticker: ', ticker ) )
    browser()
  }
  d
}

data.closePriceDataFrame <- function( ticker, d ) {
  data.frame(
    ticker = ticker,
    date = d$Date,
    close = d$Close
  )
}

data.nyse <- function( ticker ) {
  paste0( 'GOOG/NYSE_', ticker )
}

data.nasdaq <- function( ticker ) {
  paste0( 'GOOG/NASDAQ_', ticker )
}

data.addSP500 <- function( d ) {
  d.spQuandl <- Quandl( 'YAHOO/INDEX_GSPC', collapse = 'annual' )
  d.sp <- data.closePriceDataFrame( 'S&P 500', d.spQuandl )
  rbind( d, d.sp )
}

data.price <- function() {
  data.getAndCleanCSV( './data/price.csv' )
}

# data.cxAndPerformance <- function() {
#   cx <- data
#   price <- data.performance()
#   d <- join( cx, price, by = 'ticker' )
#   d
# }

data.performance <- function( d = data.price() ) {
  ticker <- d$ticker
  d.list <- lapply( c( unique( ticker ) ), data.performanceOverTime )
  ldply( d.list, data.frame )
}

data.performanceOverTime <- function( ticker ) {
  d <- data.price()
  current_value = data.closeOn( '2013-12-31', ticker, d )
  old_value.1 = data.closeOn( '2012-12-31', ticker, d )
  old_value.2 = data.closeOn( '2011-12-31', ticker, d )
  old_value.3 = data.closeOn( '2010-12-31', ticker, d )
  old_value.4 = data.closeOn( '2009-12-31', ticker, d )
  old_value.5 = data.closeOn( '2008-12-31', ticker, d )
  old_value.6 = data.closeOn( '2007-12-31', ticker, d )

  one_year = data.percGrowth( current_value, old_value.1 )
  two_year = data.percGrowth( old_value.1, old_value.2 )
  three_year = data.percGrowth( old_value.2, old_value.3 )
  four_year = data.percGrowth( old_value.3, old_value.4 )
  five_year = data.percGrowth( old_value.4, old_value.5 )
  six_year = data.percGrowth( old_value.5, old_value.6 )

  data.frame(
    ticker = ticker, one_year = one_year,  two_year = two_year,
    three_year = three_year, four_year = four_year, five_year = five_year,
    six_year = six_year,
    cumulative_return = ( 1 + one_year ) * ( 1 + two_year ) * ( 1 + three_year ) *
      ( 1 + four_year ) * ( 1 + five_year ) * ( 1 + six_year )
  )
}

# data.performanceOverTime <- function( ticker ) {
#   d <- data.price()
#   current_value = data.closeOn( '2013-12-31', ticker, d )
#   old_value.1 = data.closeOn( '2012-12-31', ticker, d )
#   old_value.2 = data.closeOn( '2011-12-31', ticker, d )
#   old_value.3 = data.closeOn( '2010-12-31', ticker, d )
#   old_value.4 = data.closeOn( '2009-12-31', ticker, d )
#   old_value.5 = data.closeOn( '2008-12-31', ticker, d )
#   old_value.6 = data.closeOn( '2007-12-31', ticker, d )

#   one_year = data.percGrowth( current_value, old_value.1 )
#   two_year = data.percGrowth( current_value, old_value.2 )
#   three_year = data.percGrowth( current_value, old_value.3 )
#   four_year = data.percGrowth( current_value, old_value.4 )
#   five_year = data.percGrowth( current_value, old_value.5 )
#   six_year = data.percGrowth( current_value, old_value.6 )

#   data.frame(
#     ticker = ticker, one_year = one_year,  two_year = two_year,
#     three_year = three_year, four_year = four_year, five_year = five_year,
#     six_year = six_year,
#     cumulative_return = ( 1 + one_year ) * ( 1 + two_year ) * ( 1 + three_year ) *
#       ( 1 + four_year ) * ( 1 + five_year ) * ( 1 + six_year )
#   )
# }

data.closeOn <- function( year, ticker, d ) {
  d[ d$ticker == ticker & d$date == as.Date( year ) ,]$close
}

data.percGrowth <- function( current_value, old_value ) {
  ( ( current_value - old_value ) / current_value )
  # ( ( current_value - old_value ) / old_value ) # http://www.investopedia.com/terms/c/cumulativereturn.asp
}

data.top10 <- function() {
  d <- data
  d.14 <- d[ d$cx_survey_year == 2014, ]
  d.14 <- d.14[ order( d$cxi, decreasing = TRUE ), ]
  d.14[ d.14$ticker != '', ][1:10,] # USAA is privately held & has top 3 positions
}

data.bottom10 <- function() {
  d <- data
  d.14 <- d[ d$cx_survey_year == 2014, ]
  d.14 <- d.14[ order( d$cxi ), ]
  d.14[ d.14$ticker != '', ][1:10,] # Medicaid = govt organisation, Anthem Blue Cross Blue Shield & Blue Cross Blue Shield of Texas & Other Blue Cross Shield = independent licensee of Blue Cross Blue Shield Assoc
}

# data.calcPerformance <- function( d ) {
#   dPerformance <- data.performance( d )
#   d <- join( d, dPerformance)
#   # d <- select( d, company, cxi, three_year, four_year, five_year, six_year)
#   # d <- melt( d, id = c ( 'company', 'cxi' ) )
#   # names( d )[3] <- 'year'
#   # names( d )[4] <- 'cumulative_return'
#   # d$year <- lapply( d$year, data.returnYear)
#   # d$year <- as.numeric( d$year )
#   d
# }

data.top10Performance <- function() {
  data.calcPerformance( data.top10() )
}

data.bottom10Performance <- function() {
  data.calcPerformance( data.bottom10() )
}

data.returnYear <- function( year_factor ) {
  if ( year_factor == 'one_year' ) 1
  else if ( year_factor == 'two_year' ) 2
  else if ( year_factor == 'three_year' ) 3
  else if ( year_factor == 'four_year' ) 4
  else if ( year_factor == 'five_year' ) 5
  else if ( year_factor == 'six_year' ) 6
}