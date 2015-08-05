# @file Encryption.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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
  rJava::J("org.ohdsi.sharing.Encryption")$generateKeyPair(publicKeyFileName, privateKeyFileName)
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
  rJava::J("org.ohdsi.sharing.Encryption")$encryptFile(sourceFileName, targetFileName, publicKeyFileName)
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
  rJava::J("org.ohdsi.sharing.Encryption")$decryptFile(sourceFileName, targetFileName, privateKeyFileName)
}
