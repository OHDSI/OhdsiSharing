context("encryption")

test_that("encryption works", {
  publicKeyFileName <- tempfile()
  privateKeyFileName <- tempfile()
  expect_null(OhdsiSharing::generateKeyPair(publicKeyFileName, privateKeyFileName))
  
  originalFile <- tempfile(fileext = ".csv")
  write.csv(mtcars, file = originalFile)
  expect_true(all.equal(mtcars, read.csv(originalFile, row.names = 1)))
  
  
  encryptedFile <- tempfile()
  expect_null(OhdsiSharing::encryptFile(originalFile, encryptedFile, publicKeyFileName))
  
  decryptedFile <- tempfile(fileext = ".csv")
  expect_null(decryptFile(encryptedFile, decryptedFile, privateKeyFileName))
  
  expect_true(all.equal(read.csv(originalFile, row.names = 1), 
                        read.csv(decryptedFile, row.names = 1)))
  
  sourceFolder <- file.path(tempdir(), "csv")
  if (!dir.exists(sourceFolder)){
    dir.create(sourceFolder)
  }
  
  write.csv(mtcars, file = file.path(sourceFolder, "mtcars.csv"))
  expect_equal(list.files(sourceFolder), "mtcars.csv")
  
  encryptedFolder <- tempfile()
  expect_null(OhdsiSharing::compressAndEncryptFolder(sourceFolder, encryptedFolder, publicKeyFileName))
  
  decryptedFolder <- file.path(tempdir(), "decrypted")
  expect_null(OhdsiSharing::decryptAndDecompressFolder(encryptedFolder, decryptedFolder, privateKeyFileName))
  
  
  expect_equal(list.files(decryptedFolder), "mtcars.csv")
  
  expect_true(all.equal(read.csv(file.path(sourceFolder, "mtcars.csv"), row.names = 1), 
                        read.csv(file.path(decryptedFolder, "mtcars.csv"), row.names = 1)))
  
  
})
