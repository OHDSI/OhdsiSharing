setupTestDatabaseDownloadJdbcDriver <- function() {
  jdbcDriverFolder <- tempfile("jdbcDrivers")
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)
  return(jdbcDriverFolder)
}

setupTestDatabaseConnectionDetails <- function(jdbcDriverFolder) {
  testDatabaseConnectionDetails <-
    DatabaseConnector::createConnectionDetails(
      dbms = "postgresql",
      user = Sys.getenv("CDM5_POSTGRESQL_USER"),
      password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
      server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
      pathToDriver = jdbcDriverFolder
    )
  return(testDatabaseConnectionDetails)
}

setupTestDatabaseConnection <- function(testDatabaseConnectionDetails) {
  return(DatabaseConnector::connect(connectionDetails = testDatabaseConnectionDetails))
}

testDatabaseJdbcDriverFolder <- setupTestDatabaseDownloadJdbcDriver()
testDatabaseConnectionDetails <- setupTestDatabaseConnectionDetails(testDatabaseJdbcDriverFolder)
testDatabaseConnection <- setupTestDatabaseConnection(testDatabaseConnectionDetails)

withr::defer({
  DatabaseConnector::disconnect(testDatabaseConnection)
  unlink(testDatabaseJdbcDriverFolder, recursive = TRUE, force = TRUE)
}, testthat::teardown_env())