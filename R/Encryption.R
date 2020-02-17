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

#' Create a public-private key pair
#'
#' @details
#' Creates an RSA 4096-bit public-private key pair. The public key can be used to encrypt data, and
#' only with the private key can the data be decrypted.
#'
#' @param publicKeyFileName    Name of the file where the public key should be stored.
#' @param privateKeyFileName   Name of the file where the private key should be stored.
#'
#' @examples
#' \dontrun{
#' generateKeyPair("public.key", "private.key")
#' }
#'
#' @export
generateKeyPair <- function(publicKeyFileName, privateKeyFileName) {
  rJava::J("org.ohdsi.sharing.Encryption")$generateKeyPair(path.expand(publicKeyFileName), path.expand(privateKeyFileName))
}

#' Encrypt a data file
#'
#' @details
#' Encrypts the data using the provided public key.
#'
#' @param sourceFileName      Name of the file that must be encrypted.
#' @param targetFileName      Name of the file that will hold the encrypted data.
#' @param publicKeyFileName   Name of the file where the public key is stored.

#' @examples
#' \dontrun{
#' generateKeyPair("public.key", "private.key")
#' data <- data.frame(x = runif(1000), y = 1:1000)
#' saveRDS(data, "data.rds")
#' encryptFile("data.rds", "data.rds.enc", "public.key")
#' }
#'
#' @export
encryptFile <- function(sourceFileName, targetFileName, publicKeyFileName) {
  rJava::J("org.ohdsi.sharing.Encryption")$encryptFile(path.expand(sourceFileName), path.expand(targetFileName), path.expand(publicKeyFileName))
}

#' Decrypt a data file
#'
#' @details
#' Decrypts the data using the provided private key.
#'
#' @param sourceFileName       Name of the file that must be decrypted.
#' @param targetFileName       Name of the file that will hold the unencrypted data.
#' @param privateKeyFileName   Name of the file where the private key is stored.
#'
#' @examples
#' \dontrun{
#' generateKeyPair("public.key", "private.key")
#' data <- data.frame(x = runif(1000), y = 1:1000)
#' saveRDS(data, "data.rds")
#' encryptFile("data.rds", "data.rds.enc", "public.key")
#' decryptFile("data.rds.enc", "data2.rds", "private.key")
#' }
#'
#' @export
decryptFile <- function(sourceFileName, targetFileName, privateKeyFileName) {
  rJava::J("org.ohdsi.sharing.Encryption")$decryptFile(path.expand(sourceFileName), path.expand(targetFileName), path.expand(privateKeyFileName))
}

#' Compress and encrypt a folder
#'
#' @details
#' Compresses all files in a folder and its subfolders, and encrypts using the provided public key.
#'
#' @param sourceFolder        Name of the folder that must be encrypted.
#' @param targetFileName      Name of the file that will hold the encrypted data.
#' @param publicKeyFileName   Name of the file where the public key is stored.

#' @examples
#' \dontrun{
#' generateKeyPair("public.key", "private.key")
#'
#' # Create a folder with some data
#' dir.create("test")
#' data <- data.frame(x = runif(1000), y = 1:1000)
#' saveRDS(data, "test/data1.rds")
#' saveRDS(data, "test/data2.rds")
#'
#' compressAndEncryptFolder("test", "data.zip.enc", "public.key")
#' decryptAndDecompressFolder("data.zip.enc", "test2", "private.key")
#' }
#'
#' @export
compressAndEncryptFolder <- function(sourceFolder, targetFileName, publicKeyFileName) {
  rJava::J("org.ohdsi.sharing.Encryption")$compressAndEncryptFolder(path.expand(sourceFolder), path.expand(targetFileName), path.expand(publicKeyFileName))
}

#' Decrypt and decompress a folder
#'
#' @details
#' Decrypts the data using the provided private key and extracts all files to a folder.
#'
#' @param sourceFileName       Name of the file that must be decrypted.
#' @param targetFolder         Name of the folder that will hold the unencrypted data.
#' @param privateKeyFileName   Name of the file where the private key is stored.
#'
#' @examples
#' \dontrun{
#' generateKeyPair("public.key", "private.key")
#'
#' # Create a folder with some data
#' dir.create("test")
#' data <- data.frame(x = runif(1000), y = 1:1000)
#' saveRDS(data, "test/data1.rds")
#' saveRDS(data, "test/data2.rds")
#'
#' compressAndEncryptFolder("test", "data.zip.enc", "public.key")
#' decryptAndDecompressFolder("data.zip.enc", "test2", "private.key")
#' }
#'
#' @export
decryptAndDecompressFolder <- function(sourceFileName, targetFolder, privateKeyFileName) {
  rJava::J("org.ohdsi.sharing.Encryption")$decryptAndDecompressFolder(path.expand(sourceFileName), path.expand(targetFolder), path.expand(privateKeyFileName))
}

#' Compress a folder
#'
#' @details
#' Compresses all files in a folder and its subfolders, and stores it in a single zip file.
#'
#' @param sourceFolder     Name of the folder that must be compressed.
#' @param targetFileName   Name of the file that will hold the compressed data.

#' @examples
#' \dontrun{
#' # Create a folder with some data
#' dir.create("test")
#' data <- data.frame(x = runif(1000), y = 1:1000)
#' saveRDS(data, "test/data1.rds")
#' saveRDS(data, "test/data2.rds")
#'
#' compressFolder("test", "data.zip")
#' decompressFolder("data.zip", "test2")
#' }
#'
#' @export
compressFolder <- function(sourceFolder, targetFileName) {
  rJava::J("org.ohdsi.sharing.Encryption")$compressFolder(path.expand(sourceFolder), path.expand(targetFileName))
}

#' Decompress a folder
#'
#' @details
#' Extracts all compressed files to a folder.
#'
#' @param sourceFileName   Name of the file that must be decompressed.
#' @param targetFolder     Name of the folder that will hold the extracted data.

#' @examples
#' \dontrun{
#' # Create a folder with some data
#' dir.create("test")
#' data <- data.frame(x = runif(1000), y = 1:1000)
#' saveRDS(data, "test/data1.rds")
#' saveRDS(data, "test/data2.rds")
#'
#' compressFolder("test", "data.zip")
#' decompressFolder("data.zip", "test2")
#' }
#'
#' @export
decompressFolder <- function(sourceFileName, targetFolder) {
  rJava::J("org.ohdsi.sharing.Encryption")$decompressFolder(path.expand(sourceFileName), path.expand(targetFolder))
}
