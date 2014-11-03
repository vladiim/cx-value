data.getAndCleanCXiData <- function() {
  d <- read.csv( './data/cx_index.csv', header = TRUE, sep = ',')
  names( d ) <- normVarNames( names( d ) )
  d$ticker <- as.character( d$ticker )
  d
}