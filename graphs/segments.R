graph.topBottom10 <- function() {
  d <- data.topBottom10()
  ggplot( d, aes( x = cumulative_return, y = cix ) )
}