data.annualMarcap <- function() {
  ticker <- data[ data$ticker != '', ]$ticker
  d.list <- lapply( c( ticker ), data.annualMarcapFromTicker )
  ldply( d.list, data.frame )
}

data.annualMarcapFromTicker <- function( ticker ) {
  d <- data.getMarcapData( ticker )
  data.marcapDataFrame( ticker, d )
}

data.getMarcapData <- function( ticker ) {
  d <- try( Quandl( data.damodaran( ticker ), collapse = 'annual' ), silent = TRUE )
  if ( class( d ) == 'try-error' ) {
    cat( paste0( 'Quandl call failed with ticker: ', ticker ) )
  }
  d
}

data.marcapDataFrame <- function( ticker, d ) {
  names( d ) <- normVarNames( names( d ) )
  data.frame(
    ticker = ticker,
    date = d$date,
    marcap = d$market_capitalization * 1000000 # units in millions of dollars: https://www.quandl.com/DMDRN/LCCI_MKT_CAP-LCC-Intl-Inc-LCCI-Market-Capitalization
  )
}

data.damodaran <- function( ticker ) {
  paste0( 'DMDRN/', ticker, '_MKT_CAP' )
}

data.marcap <- function() {
  data.getAndCleanCSV( './data/marcap.csv' )
}

# data.marcapOverTime <- function( ticker ) {
#   d <- data.marcap()
#   current_value = data.marcapOn( '2013-12-31', ticker, d )
#   old_value.1 = data.marcapOn( '2012-12-31', ticker, d )
#   old_value.2 = data.marcapOn( '2011-12-31', ticker, d )
#   old_value.3 = data.marcapOn( '2010-12-31', ticker, d )
#   old_value.4 = data.marcapOn( '2009-12-31', ticker, d )
#   old_value.5 = data.marcapOn( '2008-12-31', ticker, d )
#   old_value.6 = data.marcapOn( '2007-12-31', ticker, d )
# }