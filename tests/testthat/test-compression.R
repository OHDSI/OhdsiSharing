context("compression")

test_that("compression works",{
  
  sourceFolder <- file.path(tempdir(), "toCompress")
  if (!dir.exists(sourceFolder)){
    dir.create(sourceFolder)
  }
  
  write.csv(mtcars, file = file.path(sourceFolder, "mtcars.csv"))
  expect_equal(list.files(sourceFolder), "mtcars.csv")
  
  compressedFolder <- tempfile()
  expect_null(OhdsiSharing::compressFolder(sourceFolder, compressedFolder))
  
  decompressedFolder <- file.path(tempdir(), "decompressed")
  expect_null(OhdsiSharing::decompressFolder(compressedFolder, decompressedFolder))
  
  expect_equal(list.files(decompressedFolder), "mtcars.csv")
  
  expect_true(all.equal(read.csv(file.path(sourceFolder, "mtcars.csv"), row.names = 1), 
                        read.csv(file.path(decompressedFolder, "mtcars.csv"), row.names = 1)))
  
})