library(testthat)

context("sftp")


randVar <- rawToChar(as.raw(sample(c(65:90,97:122), 5, replace=T)))
appendRandom <- function(x, rand = randVar){
  return(paste(rand, x, sep='_'))
}


key <- Sys.getenv('SFTP_PRIVATE_KEY')
tempfileMain <- tempfile('test')
write(key, file = tempfileMain)

username <- Sys.getenv('SFTP_USER') 

sftpConnection <- sftpConnect(privateKeyFileName = tempfileMain , 
                              userName = username)

test_that("sftpConnect privateKeyFileName not exists error", { 
  
  expect_error(sftpConnect(privateKeyFileName = 'F:/madeup/nofilehere', userName = username))
  
})

test_that("sftpConnect random username error", { 
  
  expect_error(sftpConnect(privateKeyFileName = 'F:/madeup/nofilehere', userName = 'asadsfjfjdf'))
  
})

test_that("sftpConnect RSA PRIVATE KEY not in key error", { 
  
  # make key that is incorrect:
  key <- 'sometext here'
  tempfile <- tempfile('test')
  write(key, file = tempfile)
  
  expect_error(sftpConnect(privateKeyFileName = tempfile , userName = userName))
  
})


test_that("sftpConnect RSA PRIVATE KEY in key but invalid key error", { 
  
  # make key that is incorrect:
  key <- ' RSA PRIVATE KEY sometext here'
  tempfile <- tempfile('test')
  write(key, file = tempfile)
  
  expect_error(sftpConnect(privateKeyFileName = tempfile , userName = username))
  
})


test_that("sftpConnect works", { 
  expect_true(class(sftpConnection)== 'SftpConnection')
})

test_that("sftpDisconnect works", { 
  
  sftpConnection2 <- sftpConnect(privateKeyFileName = tempfileMain , 
                                userName = username)
  
  expect_true(is.null(sftpDisconnect(sftpConnection2)))
  
  expect_error(sftPwd(sftpConnection = sftpConnection2))
})


test_that("sftpDisconnect incorrect input error", { 
  
  expect_error(sftpDisconnect(list()))
  
})



# uploading

test_that("sftpPutFile sftpConnection input error", { 
  
  tempFile <- tempfile('test')
  write(x = '  ', file = tempFile)
  
  expect_error(sftpPutFile(sftpConnection = list(), 
                           localFileName = tempFile))
  
})

test_that("sftpPutFile localFileName input does not exist error", { 
  
  expect_error(sftpPutFile(sftpConnection = sftpConnection, 
                           localFileName = tempfile('test')))
  
})
  
  
test_that("sftpPutFile runs without error", { 
  
  tempFile <- tempfile('test')
  write(x = 'empty', file = tempFile)
  
  expect_true(is.null(sftpPutFile(sftpConnection = sftpConnection, 
                           localFileName = tempFile,
                           remoteFileName = appendRandom('testEmpty'))))
  
})


test_that("sftpGetFiles sftpConnection input error", { 
  
  tempdir <- tempdir('test')
  expect_error(sftpGetFiles(sftpConnection = list(), 
                           remoteFileNames = 'testEmpty',
                           localFolder = tempdir, 
                           localFileNames = file.path(tempdir, 'testEmpty')))
  
})

test_that("sftpGetFiles works", { 
  
  tempdir <- tempdir('test')
  sftpGetFiles(sftpConnection = sftpConnection, 
                            remoteFileNames = appendRandom('testEmpty'),
                            localFolder = tempdir, 
                            localFileNames = file.path(tempdir, 'testEmpty'))
  
  expect_true(file.exists(file.path(tempdir, 'testEmpty')))
  testEmptyContent <- readChar(file.path(tempdir, 'testEmpty'), file.info(file.path(tempdir, 'testEmpty'))$size)
  expect_true(grep('empty',testEmptyContent)==1)
  
})

test_that("sftpGetFiles works renames", { 
  
  tempdir <- tempdir('test')
  sftpGetFiles(sftpConnection = sftpConnection, 
               remoteFileNames = appendRandom('testEmpty'),
               localFolder = tempdir, 
               localFileNames = file.path(tempdir, 'testEmptyRe'))
  
  expect_true(file.exists(file.path(tempdir, 'testEmptyRe')))
  
})




# directory 


# get working directory
test_that("sftPwd sftpConnection input error", { 
  expect_error(sftPwd(sftpConnection = list()))
})

test_that("sftPwd runs", { 
  
  res <- sftPwd(sftpConnection = sftpConnection)
  expect_true(is.character(res))
  
})

# change working directory

test_that("sftpCd sftpConnection input error", { 
  expect_error(sftpCd(sftpConnection = list(), remoteFolder = './'))
})

test_that("sftpCd runs", { 
  
  sftpMkdir(sftpConnection = sftpConnection, 
            remoteFolder = appendRandom('cdTest'))
  
  res <- sftpCd(sftpConnection = sftpConnection,
                remoteFolder = appendRandom('cdTest'))
  expect_true(is.null(res))
  
  # check with getDir
  res <- sftPwd(sftpConnection = sftpConnection)
  expect_equal(res, paste0('/',appendRandom('cdTest')))
  
  sftpCd(sftpConnection = sftpConnection,
         remoteFolder = '/')
})

# making directory
test_that("sftpMkdir connection input error", { 
  
  expect_error(sftpMkdir(sftpConnection = list(), 
            remoteFolder = paste0('./',appendRandom('testDir'))))
  
})

test_that("sftpMkdir runs", { 
  
  res <- sftpMkdir(sftpConnection = sftpConnection, 
                   remoteFolder = paste0('./',appendRandom('testDir')))
  expect_true(is.null(res))
  
})

# removing directory
test_that("sftpRmdir connection input error", { 
  
  expect_error(sftpRmdir(sftpConnection = list(), 
                         remoteFolder = './testDir'))
  
})

test_that("sftpRmdir runs", { 
  
  res <- sftpRmdir(sftpConnection = sftpConnection, 
                   remoteFolder = paste0('./',appendRandom('testDir')))
  expect_true(is.null(res))
  
})

# error calling sftpRmdir when directory doesnt exist?
# NEED better warnings
test_that("sftpRmdir error when directory not exists (previously deleted)", { 
  
  expect_error(sftpRmdir(sftpConnection = sftpConnection, 
                         remoteFolder = paste0('./',appendRandom('testDir'))))
  
})



# extracting data 
test_that("sftpLs connection input error", { 
  expect_error(sftpLs(sftpConnection = list(), 
         remoteFolder = "./"))
})


test_that("sftpLs returns empty data.frame when dir empty", { 
  
  
  #create an empty dir
  sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
  sftpMkdir(sftpConnection = sftpConnection, remoteFolder = paste0("./",appendRandom("emptyDir")))
    
  result <- sftpLs(sftpConnection = sftpConnection, 
                      remoteFolder = paste0("./",appendRandom("emptyDir")))
  
  
  expect_true(nrow(result)==0)
  expect_true(class(result) == 'data.frame')
  
})


test_that("sftpLs returns populated data.frame when dir not empty", { 
  
  result <- sftpLs(sftpConnection = sftpConnection, 
                   remoteFolder = "./")
  
  expect_true(nrow(result)>0)
  expect_true(class(result) == 'data.frame')
  
})
  



test_that("sftpRm error with incorrect connection", { 
  
  expect_error(sftpRm(sftpConnection = list(), 
                      remoteFiles = './none'))
  
})
  

test_that("sftpRm removes correctly", { 
  
  # add file
  
  tempLoc1 <- tempfile('rm')
  tempLoc2 <- tempfile('rm')
  
  write('test file 1', tempLoc1)
  write('test file 2', tempLoc2)
  
  sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
  sftpMkdir(sftpConnection = sftpConnection, remoteFolder = appendRandom('sftpRm2'))
  sftpCd(sftpConnection = sftpConnection, remoteFolder = paste0('/',appendRandom('sftpRm2')))
  
  sftpPutFile(sftpConnection = sftpConnection, 
              localFileName = tempLoc1)
  sftpPutFile(sftpConnection = sftpConnection, 
              localFileName = tempLoc2)
  
  expect_true(nrow(sftpLs(sftpConnection = sftpConnection))>=2)
  
  # remove
  sftpRm(sftpConnection = sftpConnection, 
                      remoteFiles = c(basename(tempLoc1), basename(tempLoc2)))
  
  # check not there:
  expect_true(nrow(sftpLs(sftpConnection = sftpConnection))==0)
  sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
  sftpRmdir(sftpConnection = sftpConnection, remoteFolder = appendRandom('sftpRm2'))
})




test_that("sftpRename error with incorrect connection", { 
  
  expect_error(sftpRename(sftpConnection = list(), 
                          oldRemoteFilename =  'old',
                          newRemoteFilename = 'new'))
  
})


test_that("sftpRename rename works", { 
  
  sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
  sftpMkdir(sftpConnection = sftpConnection, remoteFolder = appendRandom('renameTest'))
  sftpCd(sftpConnection = sftpConnection, remoteFolder = appendRandom('renameTest'))
  
  tempLoc1 <- tempfile('rm')
  
  write('test file 1', tempLoc1)
  
  sftpPutFile(sftpConnection = sftpConnection, 
              localFileName = tempLoc1)
  
  original <- sftpLs(sftpConnection = sftpConnection)
  
  sftpRename(sftpConnection = sftpConnection, 
             oldRemoteFilename = basename(tempLoc1), 
             newRemoteFilename = 'renameTestWorked')
  
  new <- sftpLs(sftpConnection = sftpConnection)
  expect_equal(new$fileName,"renameTestWorked")
  
})


test_that("sftpUploadFile works", { 
  
  tempFile <- tempfile('test')
  write(x = 'testing sftpUploadFile', file = tempFile)
  
  sftpUploadFile(privateKeyFileName = tempfileMain, 
                 userName = username, 
                 remoteFolder = paste0("/",appendRandom("allTest")), 
                 fileName = tempFile)
  
  sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
  sftpCd(sftpConnection = sftpConnection, remoteFolder = appendRandom('allTest'))
  resFiles <- sftpLs(sftpConnection = sftpConnection)
  sftpRm(sftpConnection = sftpConnection, remoteFiles = resFiles$fileName)
  
  expect_true(length(grep(basename(tempFile), resFiles$fileName))>0)

})


# cleanup
sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
sftpCd(sftpConnection = sftpConnection, remoteFolder = appendRandom('renameTest'))
sftpRm(sftpConnection = sftpConnection, remoteFiles = 'renameTestWorked')

sftpCd(sftpConnection = sftpConnection, remoteFolder = '/')
filesToClean <- sftpLs(sftpConnection = sftpConnection)
filesToClean <- filesToClean[grep(paste0(randVar,'_'), filesToClean$fileName),]
if(nrow(filesToClean)>0){
  
  for(i in 1:nrow(filesToClean)){
    
    if(filesToClean$type[i]=='DIR'){
      sftpRmdir(sftpConnection = sftpConnection, remoteFolder = filesToClean$fileName[i])
    }else{
      sftpRm(sftpConnection = sftpConnection, remoteFiles = filesToClean$fileName[i])
    }
    
  }
  
}

sftpDisconnect(sftpConnection)


