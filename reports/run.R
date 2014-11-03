runReport.test <- function() {
  input = './reports/templates/test.Rmd'
  output = paste0( './reports/output/test_', vars.currentTime(), '.md' )
  # knit( input, output )
  knit( input, output )
  # output pdf:
  # http://yihui.name/knitr/demo/pandoc/

  # or use pander in reports? http://rapporter.github.io/pander/
}