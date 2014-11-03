save.annualClosePriceFromTicker <- function() {
  write.csv( data.annualStockPrices(), file = './data/price.csv' )
}

save.marcapFromTicker <- function() {
  write.csv( data.annualMarcap(), file = './data/marcap.csv' )
}