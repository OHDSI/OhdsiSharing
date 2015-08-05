.testCode <- function() {
  generateKeyPair("s:/temp/public.key", "s:/temp/private.key")

  data <- data.frame(x = runif(1000), y = 1:1000)
  saveRDS(data, "s:/temp/data.rds", compress = "xz")

  encryptFile("s:/temp/data.rds", "s:/temp/data.rds.enc", "s:/temp/public.key")

  decryptFile("s:/temp/data.rds.enc", "s:/temp/data2.rds", "s:/temp/private.key")

  data2 <- readRDS("s:/temp/data.rds")

  all.equal(data, data2)
}

