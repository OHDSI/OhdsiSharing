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
#' @param sourceFile     The path to the file in the local filesystem.
#' @param bucket         The name of the bucket to put the file in.
#' @param targetPath     The path in the bucket where to place the file, e.g. "/study1/myFile.csv".
#' @param region         The region of the S3.
#' @param key            Your AWS access key.
#' @param secret         Your AWS secret access key.
#' 
#' @export
putS3File <- function(sourceFile,
                      bucket = "ohdsi-network", 
                      targetPath,
                      region = "us-east-1", 
                      key, 
                      secret) {
  if (region != "us-east-1"){
    url <- paste0("https://s3-", region, ".amazonaws.com/")
  } else {
    url <- "https://s3.amazonaws.com/"
  }
  if (bucket != "")
    url <- paste0(url, bucket)
  if (targetPath != "")
    url <- paste0(url, targetPath)
  current <- Sys.time()
  d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
  p <- httr::parse_url(url)
  action <- if (p$path == "") "/" else paste0("/", p$path)
  
  headers = list(`Content-Length` = file.size(sourceFile))
  
  canonical_headers <- c(list(host = p$hostname, `x-amz-date` = d_timestamp), headers)
  
  Sig <- aws.signature::signature_v4_auth(
    datetime = d_timestamp,
    region = region,
    service = "s3",
    verb = "PUT",
    action = action,
    canonical_headers = canonical_headers,
    request_body = sourceFile,
    key = key, secret = secret)
  
  headers$`x-amz-date` <- d_timestamp
  headers$`x-amz-content-sha256` <- Sig$BodyHash
  headers$Authorization <- Sig$SignatureHeader
  H <- do.call(httr::add_headers, headers)
  r <- httr::PUT(url, H, body = httr::upload_file(sourceFile))
  return(parse_aws_s3_response(r))
}


parse_aws_s3_response <- function(r, Sig, verbose = getOption("verbose")){
  ## Some objects have nothing to parse
  if(is.null(r$headers$`content-type`)){
    if(verbose){
      warning("Response has no body, nothing to parse")
    }
    out <- NULL
  } else {
    if(r$headers$`content-type` == "application/xml"){
      content <- httr::content(r, "text")
      response_contents <- try(XML::xmlToList(content), silent = TRUE)
      if (!inherits(response_contents, "try-error")) {
        if (!is.null(response_contents)) {
          response <- response_contents
        } else {
          response <- NULL
        }
      } else {
        ## We should only parse XML communications from Amazon, otherwise just give the response object
        response <- r
      }
    } else {
      response <- r
    }
    #raise errors if bad values are passed. 
    if (httr::http_status(r)$category == "client error") {
      httr::warn_for_status(r)
      h <- httr::headers(r)
      out <- structure(response, headers = h, class = "aws_error")
    } else {
      out <- response
    }
    
    if (inherits(out, "aws_error")) {
      attr(out, "request_canonical") <- Sig$CanonicalRequest
      attr(out, "request_string_to_sign") <- Sig$StringToSign
      attr(out, "request_signature") <- Sig$SignatureHeader
    }
  }
  out
}