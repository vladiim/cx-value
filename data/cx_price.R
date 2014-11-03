data.topBottom10 <- function() {
  leaders = data.top10Performance()
  leaders$performance = 'Leaders'
  laggards = data.bottom10Performance()
  laggards$performance = 'Laggards'
  rbind( leaders, laggards )
}