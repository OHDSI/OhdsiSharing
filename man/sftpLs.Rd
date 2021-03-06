% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sftp.R
\name{sftpLs}
\alias{sftpLs}
\title{List the files in folder on the server.}
\usage{
sftpLs(sftpConnection, remoteFolder = "./")
}
\arguments{
\item{sftpConnection}{An SftpConnection object as created by the \code{\link{sftpConnect}}
function.}

\item{remoteFolder}{The folder on the server. Defaults to the current folder.}
}
\value{
A data frame with two columns: the file names, and the file types (directory, link, or file).
}
\description{
List the files in folder on the server.
}
