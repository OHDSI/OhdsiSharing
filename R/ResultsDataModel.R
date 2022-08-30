# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiSharing
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


#' Provides a data.frame for creating a results data model specification
#'
#' @description
#' This function will provide an empty data model specification data.frame
#' that defines the minimum required fields for use in the \code{uploadResults}
#' function.
#'
#' @param verbose   When TRUE, descriptions of each field in the data.frame
#'                  are returned
#'
#' @export
getEmptyResultsDataModelSpecification <- function(verbose = FALSE) {
  df <- CohortGenerator::readCsv(system.file("resultsDataModelSpecificationDescription.csv", package="OhdsiSharing", mustWork = TRUE))
  if (verbose) {
    print(df)
  }
  returnVal <- setNames(data.frame(matrix(ncol = nrow(df), nrow = 0), stringsAsFactors = FALSE), df$columnName)
  invisible(returnVal)
}

#' Unzips a results.zip file and enforces standards required by 
#' \code{uploadResults}
#'
#' @description
#' This function will unzip the zipFile to the resultsFolder and assert
#' that the file resultsDataModelSpecification.csv exists in the resultsFolder
#' to ensure that it will work with \code{uploadResults}
#'
#' @param zipFile   The location of the .zip file that holds the results to upload
#'
#' @param resultsFolder The folder to use when unzipping the .zip file. If this folder
#'                    does not exist, this function will attempt to create the folder.
#'
#' @export
unzipResults <- function(zipFile,
                         resultsFolder) {
  checkmate::assert_file_exists(zipFile)
  if (!dir.exists(resultsFolder)) {
    dir.create(path = resultsFolder, recursive = TRUE)
  }
  rlang::inform(paste0("Unzipping ", basename(zipFile), " to ", resultsFolder))
  zip::unzip(zipFile, exdir = resultsFolder)
  checkmate::assert_file_exists(file.path(resultsFolder, "resultsDataModelSpecification.csv"))
}

#' Creates the results data model tables to use when uploading results
#'
#' @description
#' This function assumes that you have used \code{unzipResults} to extract
#' the contents of your results .zip file and that the file 
#' resultsDataModelSpecification.csv exists in the resultsFolder. This function
#' will use resultsDataModelSpecification.csv to create the tables in the schema 
#' specified.
#'
#' @template Connection
#' 
#' @param schema   The target schema to create the tables in the database
#'
#' @param resultsFolder The folder that holds the resultsDataModelSpecification.csv
#'                    file.
#'
#' @export
createResultsDataModelTables <- function(connection = NULL,
                                         connectionDetails = NULL,
                                         schema,
                                         resultsFolder) {
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }
  
  if (connection@dbms == "sqlite" & schema != "main") {
    stop("Invalid schema for sqlite, use schema = 'main'")
  }
  
  checkmate::assert_directory_exists(resultsFolder)
  
  specifications <- getResultsDataModelSpecifications(resultsFolder = resultsFolder)
  ddlSql <- createResultsDataModelDDL(schema = schema,
                                      specifications = specifications)
  sql <- SqlRender::translate(sql = ddlSql,
                              targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, sql)
}

#' An opinionated function for uploading results a database server.
#'
#' @description
#' This function will upload results that were extracted using \code{unzipResults}
#' and upload them to a database server. This function assumes that database
#' tables are already created using the \code{createResultsDataModelTables} function.
#' This function is opinionated since it enforces the following requirements:
#' 
#' 1. The results files are in .CSV format
#' 2. The name of the result file is the name of the target table in the database
#' and that the file name is in snake_case. 
#' 3. The results .CSV file has column headings in snake_case
#' 4. The column "database_id" is used to indicate the database contributing
#' the results to the result set.
#' 6. The results .zip file contains a file called 
#' "resultsDataModelSpecification.csv" that fully specifies the 
#' results data model.
#'
#'
#' @template Connection
#' 
#'                            
#' @param schema         The schema on the postgres server where the tables have been created.
#'                       See \code{createResultsDataModelTables} for more information.
#' 
#' @param resultsFolder    The path to the folder containing the results to upload.
#'                       See \code{unzipResults} for more information.
#'                       
#' @param forceOverWriteOfSpecifications  If TRUE, specifications of the phenotypes, cohort definitions, and analysis
#'                       will be overwritten if they already exist on the database. Only use this if these specifications
#'                       have changed since the last upload.
#'                       
#' @param purgeSiteDataBeforeUploading If TRUE, before inserting data for a specific databaseId all the data for
#'                       that site will be dropped. This assumes the input zip file contains the full data for that
#'                       data site.
#'                       
#' @export
uploadResults <- function(connection = NULL,
                          connectionDetails = NULL,
                          schema,
                          resultsFolder,
                          forceOverWriteOfSpecifications = FALSE,
                          purgeSiteDataBeforeUploading = TRUE) {
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }
  
  if (connection@dbms == "sqlite" & schema != "main") {
    stop("Invalid schema for sqlite, use schema = 'main'")
  }
  
  start <- Sys.time()

  # Get the results data model specification
  specifications <- getResultsDataModelSpecifications(resultsFolder = resultsFolder)
  
  invisible(lapply(X = unique(specifications$tableName), 
                   FUN = uploadTable,
                   connection = connection,
                   schema = schema,
                   resultsFolder = resultsFolder,
                   forceOverWriteOfSpecifications = forceOverWriteOfSpecifications,
                   purgeSiteDataBeforeUploading = purgeSiteDataBeforeUploading,
                   specifications = specifications))
  delta <- Sys.time() - start
  rlang::inform(paste("Uploading data took", signif(delta, 3), attr(delta, "units")))
}

# Private methods ----------------
uploadTable <- function(tableName,
                        connection,
                        schema,
                        resultsFolder,
                        forceOverWriteOfSpecifications,
                        purgeSiteDataBeforeUploading,
                        specifications) {
  csvFileName <- paste0(tableName, ".csv")
  
  if (csvFileName %in% list.files(resultsFolder)) {
    rlang::inform(paste0("Uploading file: ", csvFileName, " to table: ", tableName))
    
    # Get the primary key columns from the specification file
    primaryKey <- specifications %>%
      filter(.data$tableName == !!tableName &
               .data$primaryKey == "Yes") %>%
      select(.data$columnName) %>%
      pull()

    # Create an environment variable to hold
    # the information about the target table for
    # uploading the data
    env <- new.env()
    env$schema <- schema
    env$tableName <- tableName
    env$primaryKey <- primaryKey
    if (purgeSiteDataBeforeUploading &&
        "database_id" %in% primaryKey) {
      # Get the databaseId by reading the 1st row of data
      results <- readr::read_csv(
        file = file.path(resultsFolder, csvFileName),
        n_max = 1,
        show_col_types = FALSE)
      if (nrow(results) == 1) {
        databaseId <- results$database_id[1]
        # Remove the existing data for the databaseId
        deleteAllRecordsForDatabaseId(
          connection = connection,
          schema = schema,
          tableName = tableName,
          databaseId = databaseId
        )
      }
      # Set primaryKeyValuesInDb to NULL
      # to indicate that the primary key
      # value need not be checked since we've
      # purged the database data ahead of loading 
      # results from the file
      env$primaryKeyValuesInDb <- NULL
    } else if (length(primaryKey) > 0) {
      sql <- "SELECT DISTINCT @primary_key FROM @schema.@table_name;"
      sql <- SqlRender::render(
        sql = sql,
        primary_key = primaryKey,
        schema = schema,
        table_name = tableName
      )
      primaryKeyValuesInDb <-
        DatabaseConnector::querySql(connection, sql)
      colnames(primaryKeyValuesInDb) <-
        tolower(colnames(primaryKeyValuesInDb))
      env$primaryKeyValuesInDb <- primaryKeyValuesInDb
    }
    
    uploadChunk <- function(chunk, pos) {
      rlang::inform(paste0(
        "- Preparing to upload rows ",
        pos,
        " through ",
        pos + nrow(chunk) - 1
      ))
      
      # Ensure all column names are in lowercase
      colnames(chunk) <- tolower(colnames(chunk))
      
      # Primary key fields cannot be NULL, so for some tables convert NAs to empty or zero:
      toEmpty <- specifications %>%
        filter(
          .data$tableName == env$tableName &
            .data$emptyIsNa == "No" & grepl("varchar", .data$dataType)
        ) %>%
        select(.data$columnName) %>%
        pull()
      if (length(toEmpty) > 0) {
        chunk <- chunk %>%
          dplyr::mutate_at(toEmpty, naToEmpty)
      }
      
      toZero <- specifications %>%
        filter(
          .data$tableName == env$tableName &
            .data$emptyIsNa == "No" &
            .data$dataType %in% c("int", "bigint", "float")
        ) %>%
        select(.data$columnName) %>%
        pull()
      if (length(toZero) > 0) {
        chunk <- chunk %>%
          dplyr::mutate_at(toZero, naToZero)
      }
      
      # Check if inserting data would violate primary key constraints:
      if (!is.null(env$primaryKeyValuesInDb)) {
        primaryKeyValuesInChunk <- unique(chunk[env$primaryKey])
        duplicates <- inner_join(env$primaryKeyValuesInDb,
                                 primaryKeyValuesInChunk,
                                 by = env$primaryKey
        )
        if (nrow(duplicates) != 0) {
          if ("database_id" %in% env$primaryKey ||
              forceOverWriteOfSpecifications) {
            rlang::inform(paste0(
              "- Found ",
              nrow(duplicates),
              " rows in database with the same primary key ",
              "as the data to insert. Deleting from database before inserting."
            ))
            deleteFromServer(
              connection = connection,
              schema = env$schema,
              tableName = env$tableName,
              keyValues = duplicates
            )
          } else {
            rlang::inform(paste0(
              "- Found ",
              nrow(duplicates),
              " rows in database with the same primary key ",
              "as the data to insert. Removing from data to insert."
            ))
            chunk <- chunk %>%
              anti_join(duplicates, by = env$primaryKey)
          }
          # Remove duplicates we already dealt with:
          env$primaryKeyValuesInDb <- env$primaryKeyValuesInDb %>%
            anti_join(duplicates, by = env$primaryKey)
        }
      }
      if (nrow(chunk) == 0) {
        rlang::inform("- No data left to insert")
      } else {
        DatabaseConnector::insertTable(
          connection = connection,
          tableName = env$tableName,
          databaseSchema = env$schema,
          data = chunk,
          dropTableIfExists = FALSE,
          createTable = FALSE,
          tempTable = FALSE,
          progressBar = TRUE
        )
      }
    }
    readr::read_csv_chunked(
      file = file.path(resultsFolder, csvFileName),
      callback = uploadChunk,
      chunk_size = 1e7,
      col_types = readr::cols(),
      guess_max = 1e6,
      progress = FALSE
    )
  }
  else {
    warning(csvFileName, " not found")
  }
}

getResultsDataModelSpecifications <- function(resultsFolder) {
  fileLocation <- file.path(resultsFolder, "resultsDataModelSpecification.csv")
  checkmate::assert_file_exists(file.path(resultsFolder, "resultsDataModelSpecification.csv"))
  resultsDataModelSpecifications <- CohortGenerator::readCsv(file = fileLocation)
  return(resultsDataModelSpecifications)
}

createResultsDataModelDDL <- function(schema,
                                      specifications) {
  checkmate::assert_subset(x = c("tableName", "columnName", "dataType", "isRequired", "primaryKey"),
                           choices = names(specifications))
  tableList <- unique(specifications$tableName)
  checkmate::assert_count(length(tableList))
  ddl <- ""
  for (t in 1:length(tableList)) {
    tableName <- tableList[t]
    dataModelSubset <- specifications[specifications$tableName == tableName, ]
    
    # Loop through the columns to create the column DDL
    columns <- c()
    primaryKey <- c()
    for (i in 1:nrow(dataModelSubset)) {
      columns <- c(columns, paste(dataModelSubset$columnName[i],
                                  dataModelSubset$dataType[i],
                                  ifelse(toupper(dataModelSubset$isRequired[i]) == "YES", "NOT NULL", "NULL")))
      if (toupper(dataModelSubset$primaryKey[i]) == "YES") {
        primaryKey <- c(primaryKey, dataModelSubset$columnName[i])
      }
    }
    
    sql <- SqlRender::readSql(system.file("sql/sql_server/CreateResultTable.sql", 
                                          package = "OhdsiSharing",
                                          mustWork = TRUE))
    renderedSql <- SqlRender::render(sql = sql,
                                     results_schema = schema,
                                     table = tableName,
                                     columns = columns,
                                     primary_key = primaryKey)
    ddl <- paste(ddl, renderedSql, sep = "\n")
  }
  invisible(ddl)
}

appendNewRows <- function(data,
                          newData,
                          tableName,
                          specifications) {
  if (nrow(data) > 0) {
    primaryKeys <- specifications %>%
      dplyr::filter(.data$tableName == !!tableName &
                      .data$primaryKey == "Yes") %>%
      dplyr::select(.data$columnName) %>%
      dplyr::pull()
    newData <- newData %>%
      dplyr::anti_join(data, by = primaryKeys)
  }
  return(dplyr::bind_rows(data, newData))
}

naToEmpty <- function(x) {
  x[is.na(x)] <- ""
  return(x)
}

naToZero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

deleteFromServer <- function(connection, schema, tableName, keyValues) {
    createSqlStatement <- function(i) {
      sql <- paste0(
        "DELETE FROM ",
        schema,
        ".",
        tableName,
        "\nWHERE ",
        paste(paste0(
          colnames(keyValues), " = '", keyValues[i, ], "'"
        ), collapse = " AND "),
        ";"
      )
      return(sql)
    }
    batchSize <- 1000
    for (start in seq(1, nrow(keyValues), by = batchSize)) {
      end <- min(start + batchSize - 1, nrow(keyValues))
      sql <- sapply(start:end, createSqlStatement)
      sql <- paste(sql, collapse = "\n")
      DatabaseConnector::executeSql(
        connection,
        sql,
        progressBar = FALSE,
        reportOverallTime = FALSE,
        runAsBatch = TRUE
      )
    }
  }

deleteAllRecordsForDatabaseId <- function(connection,
                                          schema,
                                          tableName,
                                          databaseId) {
  sql <-
    "SELECT COUNT(*) FROM @schema.@table_name WHERE database_id = '@database_id';"
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    table_name = tableName,
    database_id = databaseId
  )
  databaseIdCount <-
    DatabaseConnector::renderTranslateQuerySql(connection, sql)[, 1]
  if (databaseIdCount != 0) {
    rlang::inform(
      sprintf(
        "- Found %s rows in  database with database ID '%s'. Deleting all before inserting.",
        databaseIdCount,
        databaseId
      )
    )
    sql <-
      "DELETE FROM @schema.@table_name WHERE database_id = '@database_id';"
    sql <- SqlRender::render(
      sql = sql,
      schema = schema,
      table_name = tableName,
      database_id = databaseId
    )
    DatabaseConnector::renderTranslateExecuteSql(connection,
      sql,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }
}
