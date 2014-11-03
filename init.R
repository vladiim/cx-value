# source('init.r')

# ----------- # # ----------- # # ----------- #
# DEPENDENCIES

library( Quandl )
# library( reshape )
library( knitr )
library( markdown )
library( rattle )
library( ggplot2 )
library( scales )
# library( stringr )
# library( lubridate )
library( RForcecom )
library( dplyr )

# ----------- # # ----------- # # ----------- #
# SET UP

# helper functions

loadDir <- function( dir ) {
  if ( file.exists( dir ) ) {
    message( '\n --------------------' )
    message( '# Loading dir: ', dir )
    files <- dir( dir , pattern = '[.][rR]$' )
    lapply( files, function( file ) loadFile( file, dir ) )
  }
}

loadFile <- function( file, dir ) {
  filename <- paste0( dir, '/', file )
  message( paste0( '* Loading file: ', filename ) )
  source( filename )
}

setReportingWd <- function() {
  message( paste0( '\n Working dir is: ', getwd() ) )
  if( basename( getwd() ) == 'templates' ) {
    setwd( '../../' )
    message( paste0( '\n Changing into dir: ', getwd() ) )
  }
}

# set up
dirs <- c( 'data', 'graphs', 'lib' )
lapply( dirs, loadDir )
source( './reports/run.R' )
setReportingWd()

Quandl.auth( Sys.getenv( 'QUANDL' ) )

data <- data.getAndCleanCXiData()
