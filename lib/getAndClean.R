data.getAndCleanCSV <- function( file ) {
  d <- read.csv( file, header = TRUE, sep = ',' )
  names( d ) <- normVarNames( names( d ) )
  d$date <- as.Date( d$date )
  d$ticker <- as.character( d$ticker )
  d
}