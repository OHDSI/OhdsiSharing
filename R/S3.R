# @file S3.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
uploadFileToS3 <- function(uploadFileName, bucketName, keyName,
                           accessKey = NULL, secretKey = NULL) {
  if (is.null(accessKey) || is.null(secretKey)) {
    stop("Must provide AWS access key and secret key to for S3 bucket upload")
  }
  
  Sys.setenv(AWS_ACCESS_KEY_ID = accessKey)
  Sys.setenv(AWS_SECRET_KEY = secretKey)
  
  rJava::J("org.ohdsi.sharing.UploadToS3")$uploadFile(uploadFileName, bucketName, keyName)
}
