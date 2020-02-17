# Copyright 2020 Observational Health Data Sciences and Informatics
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


#' Connect to the OHDSI SFTP server
#'
#' @param privateKeyFileName  A character string denoting the path to an RSA private key.
#' @param userName            A character string containing the user name.
#'
#' @return 
#' An SftpConnection object
#' 
#' @export
sftpConnect <- function(privateKeyFileName, userName) {
  if (!file.exists(privateKeyFileName))
    stop("Private key file ", privateKeyFileName, " does not exists.")
  key <- readChar(privateKeyFileName, file.info(privateKeyFileName)$size)
  if (!grepl("RSA PRIVATE KEY", key))
    stop("Private key file does not contain RSA private key")
  rm(key)
  userName <- as.character(userName)
  
  ParallelLogger::logInfo("Connecting to OHDSI SFTP server") 
  connection <- rJava::.jnew("org.ohdsi.sharing.Sftp", privateKeyFileName, userName)
  sftpConnection <- list(connection = connection)
  class(sftpConnection) = "SftpConnection"
  return(sftpConnection)
}

#' Disconnect from the OHDSI SFTP server.
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#'
#' @export
sftpDisconnect <- function(sftpConnection) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  sftpConnection$connection$disconnect()
  ParallelLogger::logInfo("Disconnected from OHDSI SFTP server") 
}

#' Put a file on the SFTP server
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param localFileName    The path to the local file to upload.
#' @param remoteFileName   The name the file should have on the server.
#'
#' @export
sftpPutFile <- function(sftpConnection, localFileName, remoteFileName = basename(localFileName)) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  if (!file.exists(localFileName))
    stop("Local file ", localFileName, " does not exists.")
  sftpConnection$connection$putFile(localFileName, remoteFileName)
}

#' Get a file from the SFTP server
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param localFileName    The name the file should have locally.
#' @param remoteFileName   The name the file on the server.
#'
#' @export
sftpGetFile <- function(sftpConnection, remoteFileName, localFileName = remoteFileName) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  sftpConnection$connection$getFile(remoteFileName, localFileName)
}

#' List the files in folder on the server.
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param remoteFolder    The folder on the server. Defaults to the current folder.
#' 
#' @return
#' A data frame with two columns: the file names, and the file types (directory, link, or file).
#'
#' @export
sftpLs <- function(sftpConnection, remoteFolder = "./") {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  files <- sftpConnection$connection$ls(remoteFolder)
  offset <- length(files) / 2
  files <- data.frame(fileName = files[1:offset],
                      type = as.factor(files[(offset + 1):(2*offset)]),
                      stringsAsFactors = FALSE)
  return(files)
}

#' Get the present working directory
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' 
#' @return
#' A character string representing the current remote folder name.
#'
#' @export
sftPwd <- function(sftpConnection) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  return(sftpConnection$connection$pwd())
}

#' Change the current working director
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param remoteFolder     The folder on the server to change to.
#' 
#' @export
sftpCd <- function(sftpConnection, remoteFolder) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  return(sftpConnection$connection$cd(remoteFolder))
}

#' Make a directory
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param remoteFolder     The folder on the server to create.
#' 
#' @export
sftpMkdir <- function(sftpConnection, remoteFolder) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  return(sftpConnection$connection$mkdir(remoteFolder))
}

#' Remove a directory
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param remoteFolder     The folder on the server to remove.
#' 
#' @export
sftpRmdir <- function(sftpConnection, remoteFolder) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  return(sftpConnection$connection$rmdir(remoteFolder))
}

#' Remove a file
#'
#' @param sftpConnection   An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param remoteFile       The file on the server to remove.
#' 
#' @export
sftpRm <- function(sftpConnection, remoteFile) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  return(sftpConnection$connection$rm(remoteFile))
}

#' Rename a file or folder
#'
#' @param sftpConnection     An SftpConnection object as created by the \code{\link{sftpConnect}} function.
#' @param oldRemoteFilename  The file on the server to rename.
#' @param newRemoteFilename  The new file name.
#' 
#' @export
sftpRename <- function(sftpConnection, oldRemoteFilename, newRemoteFilename) {
  if (!class(sftpConnection) == "SftpConnection")
    stop("Argument is not of type SftpConnection")
  return(sftpConnection$connection$rename(oldRemoteFilename, newRemoteFilename))
}

#' Upload a single file to the OHDSI SFTP server
#' 
#' @description 
#' This function combines calls to the \code{\link{sftpConnect}}, \code{\link{sftpPutFile}},
#' and \code{\link{sftpDisconnect}} functions.
#' 
#' A random string will be prefixed to the file name to prevent overwriting existing files on the server.
#'
#' @param privateKeyFileName  A character string denoting the path to an RSA private key.
#' @param userName            A character string containing the user name.
#' @param fileName            A character string denoting the path to file to upload.
#'
#' @export
sftpUploadFile <- function(privateKeyFileName, userName, fileName) {
  connection <- sftpConnect(privateKeyFileName, userName)
  on.exit(sftpDisconnect(connection))
  
  remoteFileName <- basename(fileName)
  remoteFileName <- paste(paste(sample(c(letters, 0:9), 8), collapse = ""), remoteFileName, sep = "_")
  ParallelLogger::logInfo("Uploading ", fileName, " to ", remoteFileName, " on OHDSI SFTP server")
  sftpPutFile(connection, fileName)
}
