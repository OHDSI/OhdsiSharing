library("testthat")

# getEmptyResultsDataModelSpecification Tests ----------
test_that("Test getEmptyResultsDataModelSpecification is invisible", {
  expect_invisible(getEmptyResultsDataModelSpecification())
})

test_that("Test getEmptyResultsDataModelSpecification is verbose", {
  df <- getEmptyResultsDataModelSpecification(verbose = TRUE)
  expect_equal(nrow(df), 0)
})

# uploadResults Tests ----------
test_that("Test uploadResults expected case", {
  zipFileName <- system.file("testData/resultsUploadTestData.zip", 
                             package = "OhdsiSharing",
                             mustWork = TRUE)
  schema <- "main"
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  tableCountBeforeUpload <- length(DatabaseConnector::getTableNames(connection, 
                                                                    databaseSchema = schema))
  OhdsiSharing::uploadResults(connectionDetails = connectionDetails,
                              schema = schema,
                              zipFileName = zipFileName,
                              createTables = TRUE,
                              forceOverWriteOfSpecifications = TRUE,
                              purgeSiteDataBeforeUploading = TRUE)
  tableCountAfterUpload <- length(DatabaseConnector::getTableNames(connection, 
                                                                   databaseSchema = schema))
  expect_gt(tableCountAfterUpload, tableCountBeforeUpload)
})

