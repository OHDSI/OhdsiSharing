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


# The code on this page is borrowed from aws.s3: https://github.com/cloudyr/aws.s3/blob/master/R/http.r
# We'll switch to using that package when it is stable.

#' Put a local file into a remote S3 bucket
#'
#' @param file   The path to the file in the local filesystem.
#' @param bucket       The name of the bucket to put the file in.
#' @param region       The region of the S3.
#' @param key          Your AWS access key.
#' @param secret       Your AWS secret access key.
#' @param appendUuid   Should append an universally unique identifier to file name when uploading?
#'
#' @export
putS3File <- function(file,
                      bucket,
                      region = "us-east-1",
                      key,
                      secret, 
                      appendUuid = TRUE) {
  suppressWarnings(if (!require("aws.s3", quietly = TRUE)) {
    install.packages("aws.s3", repos = "http://cloudyr.github.io/drat", quiet = TRUE)
  })
  
  object <- basename(file)
  
  if (appendUuid) {
    object <- paste0(object, ".", uuid::UUIDgenerate())
  }
  
  aws.s3::put_object(file = file, object = object, bucket = bucket, key = key, secret = secret, region = region)
}
