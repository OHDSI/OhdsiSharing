test_that("is_installed works", {
  expect_true(is_installed("base"))
  expect_false(is_installed("base", version = 100))
  expect_false(is_installed("blah"))
})

test_that("ensure_installed works", {
  expect_error(ensure_installed("blah"), "must be installed")
  expect_null(ensure_installed("base"))
})

test_that("encryption works", {
  publicKeyFileName <- tempfile()
  privateKeyFileName <- tempfile()
  expect_null(generateKeyPair(publicKeyFileName, privateKeyFileName))
  
  originalFile <- tempfile(fileext = ".csv")
  write.csv(mtcars, file = originalFile)
  expect_true(all.equal(mtcars, read.csv(originalFile, row.names = 1)))
  
  
  encryptedFile <- tempfile()
  expect_null(encryptFile(originalFile, encryptedFile, publicKeyFileName))
  
  decryptedFile <- tempfile(fileext = ".csv")
  expect_null(decryptFile(encryptedFile, decryptedFile, privateKeyFileName))
  
  expect_true(all.equal(read.csv(originalFile, row.names = 1), 
                        read.csv(decryptedFile, row.names = 1)))
  
  sourceFolder <- file.path(tempdir(), "csv")
  dir.create(sourceFolder)
  
  write.csv(mtcars, file = file.path(sourceFolder, "mtcars.csv"))
  expect_equal(list.files(sourceFolder), "mtcars.csv")
  
  encryptedFolder <- tempfile()
  expect_null(compressAndEncryptFolder(sourceFolder, encryptedFolder, publicKeyFileName))
  
  decryptedFolder <- file.path(tempdir(), "decrypted")
  expect_null(decryptAndDecompressFolder(encryptedFolder, decryptedFolder, privateKeyFileName))
  
  
  expect_equal(list.files(decryptedFolder), "mtcars.csv")
  
  expect_true(all.equal(read.csv(file.path(sourceFolder, "mtcars.csv"), row.names = 1), 
                        read.csv(file.path(decryptedFolder, "mtcars.csv"), row.names = 1)))
  
  
})

test_that("compression works",{
  
  sourceFolder <- file.path(tempdir(), "toCompress")
  dir.create(sourceFolder)
  
  write.csv(mtcars, file = file.path(sourceFolder, "mtcars.csv"))
  expect_equal(list.files(sourceFolder), "mtcars.csv")
  
  compressedFolder <- tempfile()
  expect_null(compressFolder(sourceFolder, compressedFolder))
  
  decompressedFolder <- file.path(tempdir(), "decompressed")
  expect_null(decompressFolder(compressedFolder, decompressedFolder))
  
  expect_equal(list.files(decompressedFolder), "mtcars.csv")
  
  expect_true(all.equal(read.csv(file.path(sourceFolder, "mtcars.csv"), row.names = 1), 
                        read.csv(file.path(decompressedFolder, "mtcars.csv"), row.names = 1)))
  
})