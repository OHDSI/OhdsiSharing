.testCode <- function() {
  generateKeyPair("s:/temp/public.key", "s:/temp/private.key")
  
  data <- data.frame(x = runif(1000), y = 1:1000)
  saveRDS(data, "s:/temp/data.rds", compress = "xz")
  
  encryptFile("s:/temp/data.rds", "s:/temp/data.rds.enc", "s:/temp/public.key")
  
  decryptFile("s:/temp/data.rds.enc", "s:/temp/data2.rds", "s:/temp/private.key")
  
  data2 <- readRDS("s:/temp/data.rds")
  
  all.equal(data, data2)
  
  
  
  
  if (!file.exists("s:/temp/test")){
    dir.create("s:/temp/test")
  }
  data <- data.frame(x = runif(1000), y = 1:1000)
  saveRDS(data, "s:/temp/test/data1.rds", compress = "xz")
  saveRDS(data, "s:/temp/test/data2.rds", compress = "xz")
  compressAndEncryptFolder("s:/temp/test", "s:/temp/data.zip.enc", "s:/temp/public.key")
  
  decryptAndDecompressFolder("s:/temp/data.zip.enc", "s:/temp/test2", "s:/temp/private.key")
  
  
  
  
  compressFolder("s:/temp/test", "s:/temp/data.zip")
  decompressFolder("s:/temp/data.zip", "s:/temp/test2")
}

