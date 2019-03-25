
# RMP Library

# commonly used packages that everyone should have installed
requiredPackages <-
  c(
    "dplyr"
   ,"excel.link"
   ,"openxlsx"
   ,"readxl"
   ,"RODBC"
  )

# install the required packages
installRequiredPackages <- function() {
  for(package in requiredPackages) {
    isInstalled <- require(package, character.only=T, quietly=T)
    if(!isInstalled) {
      print(paste("Installing", package))
      install.packages(package, repos = "http://cran.us.r-project.org")

      isInstalled <- require(package, character.only=T, quietly=T)
      if(!isInstalled) {
        stop(paste("ERROR: couldn't install R package ", package, ", can't continue", sep=""))
      }
    } else {
      print(paste(package, "is already installed."))
    }
  }
}

# loads all the required libraries into current R session
loadRequiredPackages <- function() {
  suppressPackageStartupMessages({
    for(package in requiredPackages) {
      success <- library(package, character.only = T, quietly = T, warn.conflicts = FALSE, logical.return = T)
      if(!success) {
        stop(paste("ERROR: Couldn't import package", package))
      }
    }
  })
}

## input and output can be individual path strings or vectors of path strings.
## returns true if output file(s) neeed to be regenerated b/c they are older than the newest input file(s).
needsUpdate <- function(input, output) {
  latestInput <- max(file.info(input)$mtime)
  earliestOutput <- min(file.info(output)$mtime)
  if(is.na(earliestOutput)) {
    earliestOutput <- 0
  }
  ##print(paste("comparing", latestInput, earliestOutput))
  return(latestInput > earliestOutput)
}

## WARNING: This is EXTREMELY slow. It took several mins to write 30,000 rows to a table.
## Use writeToTsv() instead and use bcp to load the file.
insertIntoDatabase<- function(tableName, df) {

  server <- Sys.getenv("RmpHost")
  databaseName <- Sys.getenv("RmpDatabase")

  if(server == "") {
    stop("ERROR: RmpHost env var not set, can't continue")
  }
  if(databaseName == "") {
    stop("ERROR: RmpDatabase env var not set, can't continue")
  }

  db <- odbcDriverConnect(
    paste('driver={SQL Server};server=', server, ';database=', databaseName, ';trusted_connection=true', sep=''))

  results <- sqlQuery(db, paste("select count(*) as Total from", tableName))
  rowCount <- results$Total[1]
  if(rowCount == 0) {
    print(paste("Inserting into table", tableName))
    sqlSave(db, df, tablename = tableName , rownames = F, append = T)
  } else {
    print(paste("Skipping import:", rowCount, "rows already exist in", tableName))
  }

  odbcClose(db)
}

## append base filename to R output dir
getROutputPath <- function(filename) {
  dir <- Sys.getenv("RmpROutputDir")
  return(paste(dir, filename, sep='/'))
}

## common desirable options when reading in CSV files
readCSV <- function(path, sep=",", fileEncoding="") {
  return(read.csv(
    path
   ,header=TRUE
   ,sep=sep
    ## check.names=F so we don't mangle headers that begin with numerals
   ,check.names=FALSE
   ,as.is=TRUE
   ,na.strings=c('')
    ## treat everything as text to preserve source data as closely as possible
   ,colClasses=c('character')
   ,fileEncoding=fileEncoding
  ))
}

## Write to delimited file; default to tabs
writeToDelimitedFile <- function(filename, df, sep="\t") {
  write.table(
    df,
    file = filename,
    append = FALSE,
    quote = FALSE,
    sep = sep,
    eol = "\n",
    na = "",
    dec = ".",
    row.names = FALSE,
    col.names = TRUE
  )
}

## Write to tab-separated file
writeToTsv <- function(filename, df) {
  writeToDelimitedFile(filename, df)
}
