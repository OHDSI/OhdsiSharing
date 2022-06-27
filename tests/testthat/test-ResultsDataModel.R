library("testthat")

# getEmptyResultsDataModelSpecification Tests ----------
test_that("Test getEmptyResultsDataModelSpecification is invisible", {
  expect_invisible(getEmptyResultsDataModelSpecification())
})

test_that("Test getEmptyResultsDataModelSpecification is verbose", {
  df <- getEmptyResultsDataModelSpecification(verbose = TRUE)
  expect_equal(nrow(df), 0)
})

# unzipResults Tests -----------
test_that("Test unzipResults fails when .zip file missing", {
  zipFileName <- tempfile(fileext = ".zip")
  unzipFolder <-tempfile()
  expect_error(OhdsiSharing::unzipResults(zipFile = zipFileName, unzipFolder = unzipFolder))
  unlink(unzipFolder)
})

test_that("Test unzipResults fails when resultsDataModelSpecification.csv is missing from .zip file", {
  zipFileName <- system.file("testData/missingResultsDataModelSpecification.zip",
                             package = "OhdsiSharing",
                             mustWork = TRUE)
  tempZipLocation <- tempfile()
  dir.create(tempZipLocation, recursive = TRUE)
  file.copy(from = zipFileName,
            to = tempZipLocation)
  copiedZipFile <- file.path(tempZipLocation, basename(zipFileName))
  expect_error(OhdsiSharing::unzipResults(zipFile = copiedZipFile, unzipFolder = tempZipLocation))
  unlink(tempZipLocation)
})

test_that("Test unzipResults passes when resultsDataModelSpecification.csv exists in .zip file", {
  zipFileName <- system.file("testData/resultsUploadTestData.zip",
                             package = "OhdsiSharing",
                             mustWork = TRUE)
  tempZipLocation <- tempfile()
  dir.create(tempZipLocation, recursive = TRUE)
  file.copy(from = zipFileName,
            to = tempZipLocation)
  copiedZipFile <- file.path(tempZipLocation, basename(zipFileName))
  OhdsiSharing::unzipResults(zipFile = copiedZipFile, resultsFolder = tempZipLocation)
  fileList <- list.files(tempZipLocation)
  expect_gt(length(fileList), 0)
})

# createResultsDataModelTables Tests -----------
test_that("Test createResultsDataModelTables fails when missing connection or connectionDetails", {
  expect_error(OhdsiSharing::createResultsDataModelTables(connection = NULL,
                                                          connectionDetails = NULL,
                                                          schema = "main",
                                                          resultsFolder = tempfile()))
})

test_that("Test createResultsDataModelTables fails when schema != main and using SqlLite", {
  
  expect_error(OhdsiSharing::createResultsDataModelTables(connectionDetails = Eunomia::getEunomiaConnectionDetails(),
                                                          schema = "not_main",
                                                          resultsFolder = tempfile()))
})

test_that("Test createResultsDataModelTables fails when resultsFolder is missing", {
  expect_error(OhdsiSharing::createResultsDataModelTables(connectionDetails = Eunomia::getEunomiaConnectionDetails(),
                                                          schema = "main",
                                                          resultsFolder = tempfile()))
})

test_that("Test createResultsDataModelTables passes during expected case", {
  zipFileName <- system.file("testData/resultsUploadTestData.zip",
                             package = "OhdsiSharing",
                             mustWork = TRUE)
  tempZipLocation <- tempfile()
  dir.create(tempZipLocation, recursive = TRUE)
  file.copy(from = zipFileName,
            to = tempZipLocation)
  copiedZipFile <- file.path(tempZipLocation, basename(zipFileName))
  OhdsiSharing::unzipResults(zipFile = copiedZipFile, resultsFolder = tempZipLocation)
  # Get the count of tables before creating the results data model
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  tablesBefore <- DatabaseConnector::getTableNames(connection = conn,
                                                   databaseSchema = "main")
  
  # Create the data model tables
  OhdsiSharing::createResultsDataModelTables(connectionDetails = connectionDetails,
                                             schema = "main",
                                             resultsFolder = tempZipLocation)
  
  tablesAfter <- DatabaseConnector::getTableNames(connection = conn,
                                                  databaseSchema = "main")
  
  expect_gt(length(tablesAfter), length(tablesBefore))
})

# uploadResults Tests ----------
test_that("Test uploadResults fails when missing connection or connectionDetails", {
  expect_error(OhdsiSharing::uploadResults(connection = NULL,
                                           connectionDetails = NULL,
                                           schema = "main",
                                           resultsFolder = tempfile()))
})

test_that("Test uploadResults fails when schema != main and using SqlLite", {
  expect_error(OhdsiSharing::uploadResults(connectionDetails = Eunomia::getEunomiaConnectionDetails(),
                                           schema = "not_main",
                                           resultsFolder = tempfile()))
})

test_that("Test uploadResults fails when resultsFolder is missing", {
  expect_error(OhdsiSharing::uploadResults(connectionDetails = Eunomia::getEunomiaConnectionDetails(),
                                           schema = "main",
                                           resultsFolder = tempfile()))
})

test_that("Test uploadResults expected case", {
  # Unzip the test data
  zipFileName <- system.file("testData/resultsUploadTestData.zip",
                             package = "OhdsiSharing",
                             mustWork = TRUE)
  tempZipLocation <- tempfile()
  dir.create(tempZipLocation, recursive = TRUE)
  file.copy(from = zipFileName,
            to = tempZipLocation)
  copiedZipFile <- file.path(tempZipLocation, basename(zipFileName))
  OhdsiSharing::unzipResults(zipFile = copiedZipFile, resultsFolder = tempZipLocation)
  
  # Create the tables
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  schema <- "main"
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  
  tableCountBeforeUpload <- length(DatabaseConnector::getTableNames(connection,
                                                                    databaseSchema = schema))
  
  OhdsiSharing::createResultsDataModelTables(connectionDetails = connectionDetails,
                                             schema = "main",
                                             resultsFolder = tempZipLocation)

  expect_warning(OhdsiSharing::uploadResults(connection = connection,
                                             schema = schema,
                                             resultsFolder = tempZipLocation,
                                             forceOverWriteOfSpecifications = TRUE,
                                             purgeSiteDataBeforeUploading = TRUE))
  
  tableCountAfterUpload <- length(DatabaseConnector::getTableNames(connection,
                                                                   databaseSchema = schema))
  expect_gt(tableCountAfterUpload, tableCountBeforeUpload)
})

test_that("Test createResultsDataModelDDL rejects specification data.frame missing required names", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  schema <- "main"
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  resultsFolder <- system.file("testData/invalidSpecTest",
                               package = "OhdsiSharing",
                               mustWork = TRUE)
  
  expect_error(createResultsDataModelTables(connection = connection,
                                            schema = schema,
                                            resultsFolder = resultsFolder))
})

