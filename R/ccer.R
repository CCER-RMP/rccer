
# RMP Library

# commonly used packages that everyone should have installed
requiredPackages <-
  c(
    "dplyr"
   ,"excel.link"
   ,"openxlsx"
   ,"purrr"
   ,"readr"
   ,"readxl"
   ,"RODBC"
   ,"stringr"
   ,"tibble"
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
  return(read_delim(path,
    sep,
    # override default 'na' to avoid interpreting 'NA' string as nulls:
    # 'NA' string is an actually occurring value we need to preserve in some of our data
    na = c(""),
    trim_ws = FALSE,
    col_types = cols(.default = "c")))
}

## Write to delimited file; default to tabs
writeToDelimitedFile <- function(filename, df, sep="\t", fileEncoding="") {
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
    col.names = TRUE,
    fileEncoding = fileEncoding
  )
}

## Write to tab-separated file
writeToTsv <- function(filename, df, fileEncoding="") {
  writeToDelimitedFile(filename, df, fileEncoding="")
}


#' Generates per-column stats on a SQL Server database table for
#' data validation purposes.
#' 
#' Operates on a database table instead of an R dataframe b/c
#' the subsequent deeper digging we typically want to do is a bit easier
#' in SQL. Future work might include changing this to work for dataframes.
#' 
#' @param schema schema
#' @param table table
#' @param server server host name; if none specified, tries to use env var
#' @param db database; if none specified, tries to use env var
#' @return dataframe where rows contain information about each 
#'   table column being analyzed, and columns have different stats
#'   on the data, such as number of distinct values, number of nulls, etc.
tableStats <- function(schema, table, server, db, verbose=FALSE) {

  if(missing(server)) {
    server <- Sys.getenv("RmpHost")
  }

  if(missing(db)) {
    db <- Sys.getenv("RmpDatabase")
  }
  
  full_table_name <- paste('[', schema, '].[', table, ']', sep='')
  
  queries <- list(
    list(name='NumDistinct',
         sql_template=paste("SELECT count(distinct COLUMN_NAME) AS N FROM TABLE_NAME", sep=''),
         on_column_types=c("all"),
         type="numeric")
    ,list(name='NumNulls',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME IS NULL", sep=''),
          on_column_types=c("all"),
          type="numeric")
    ,list(name='NumEmptyStr',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME = ''", sep=''),
          on_column_types=c("varchar", "char", "text"),
          type="numeric")
    ,list(name='NumNullLiterals',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME = 'NULL'", sep=''),
          on_column_types=c("varchar", "char", "text"),
          type="numeric")
    ,list(name='NumQuotes',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME LIKE '\"%' OR COLUMN_NAME LIKE '%\"'", sep=''),
          on_column_types=c("varchar", "char", "text"),
          type="numeric")
    ,list(name='NumExtraWhitespace',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME LIKE ' %' OR COLUMN_NAME LIKE '% '", sep=''),
          on_column_types=c("varchar", "char", "text"),
          type="numeric")
    ,list(name='NumNumeric',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME IS NOT NULL and ISNUMERIC(COLUMN_NAME) = 1", sep=''),
          on_column_types=c("varchar", "char", "text", "int", "numeric", "tinyint", "bigint", "float"),
          type="numeric")
    ,list(name='NumNonNumeric',
          sql_template=paste("SELECT count(*) AS N FROM TABLE_NAME WHERE COLUMN_NAME IS NOT NULL and ISNUMERIC(COLUMN_NAME) = 0", sep=''),
          on_column_types=c("varchar", "char", "text", "int", "numeric", "tinyint", "bigint", "float"),
          type="numeric")
    ,list(name='MinLength',
          sql_template=paste("SELECT min(len(COLUMN_NAME)) AS N FROM TABLE_NAME", sep=''),
          on_column_types=c("all"),
          type="numeric")
    ,list(name='MaxLength',
          sql_template=paste("SELECT max(len(COLUMN_NAME)) AS N FROM TABLE_NAME", sep=''),
          on_column_types=c("all"),
          type="numeric")
    ,list(name='MinValue',
          sql_template=paste("SELECT min(COLUMN_NAME) AS N FROM TABLE_NAME", sep=''),
          on_column_types=c("varchar", "char", "text", "int", "numeric", "tinyint", "bigint", "float"), # exclude bit
          type="character")
    ,list(name='MaxValue',
          sql_template=paste("SELECT max(COLUMN_NAME) AS N FROM TABLE_NAME", sep=''),
          on_column_types=c("varchar", "char", "text", "int", "numeric", "tinyint", "bigint", "float"), # exclude bit
          type="character")
  )

  db <- odbcDriverConnect(
    paste('driver={SQL Server};server=', server, ';database=', db, ';trusted_connection=true', sep=''))

  sql <- paste("
  SELECT
    ColumnName = COLUMN_NAME
    ,DataTypeBase = DATA_TYPE
    ,DataTypeFull = CONCAT(DATA_TYPE, 
    CASE 
      WHEN DATA_TYPE LIKE '%CHAR%' THEN CONCAT('(', CHARACTER_MAXIMUM_LENGTH,')') 
      WHEN DATA_TYPE = 'FLOAT' THEN CONCAT('(', NUMERIC_PRECISION,')') 
      WHEN DATA_TYPE IN ('NUMERIC', 'DECIMAL') THEN CONCAT('(', NUMERIC_PRECISION, ',', NUMERIC_SCALE, ')')
      ELSE ''
    END)
  FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '", table, "' AND TABLE_SCHEMA = '", schema, "'
  ORDER BY ORDINAL_POSITION", sep='')
  
  columns <- sqlQuery(db, sql, as.is=c(TRUE))
  
  # define fns here so they are closures over this scope
  exec_query <- function(column, query) {
    query_name <- query[["name"]]
    sql_template <- query[["sql_template"]]
    on_column_types <- query[["on_column_types"]]
    type <- query[["type"]]
    
    if(verbose) {
      print(paste("Generating result for", query_name, "on", column))
    }
    
    # if query isn't appropriate for this column type, skip query
    if(!("all" %in% on_column_types)) {
      if(!(subset(columns, ColumnName==column)[1, 'DataTypeBase'] %in% on_column_types)) {
        return(NA)
      }
    }

    sql <- sql_template
    sql <- stringr::str_replace_all(sql, 'COLUMN_NAME', paste('[', column, ']', sep=''))
    sql <- stringr::str_replace_all(sql, 'TABLE_NAME', full_table_name)

    # need to specify as.is, otherwise R will convert chars to numeric when it can,
    # which gives us misleading MinValue
    results <- sqlQuery(db, sql, as.is=c(TRUE))
    if(class(results) != "data.frame") {
      stop(paste("SQL ERROR:", results))
    }
    n <- results[1, 1]
    if(type == "character") {
      n <- as.character(n)
    }
    return(n)
  }
  
  do_query <- function(query) {
    fn <- purrr::partial(exec_query, query = query)
    results <- sapply(as.character(columns$ColumnName), fn)
    return(results)
  }
  
  # lapply returns list of vectors; sapply returns matrix, which flattens all datatypes
  all_results <- data.frame(lapply(queries, do_query), stringsAsFactors=FALSE)

  # to get column names, transform into a matrix and get 1st column
  colnames(all_results) <- do.call(rbind, queries)[,1]

  columns_to_select <- c(
    "ColumnName",
    "DataType",
    unlist(unname(sapply(queries, "[", "name")))
  )

  all_results <- tibble::rownames_to_column(all_results, "ColumnName") %>%
    inner_join(columns) %>%
    rename(DataType = DataTypeFull) %>%
    select(columns_to_select)

  odbcClose(db)
  
  return(all_results)
}
