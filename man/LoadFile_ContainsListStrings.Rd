% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FileHandling.R
\name{LoadFile_ContainsListStrings}
\alias{LoadFile_ContainsListStrings}
\title{LoadFile_ContainsListStrings}
\usage{
LoadFile_ContainsListStrings(Dir.Path, StringsInFileName,
  Read.Function = read.csv)
}
\arguments{
\item{Dir.Path}{The path to the directory in which to search}

\item{StringsInFileName}{The vector of strings to search for}

\item{Read.Function}{What to use to load the file e.g. read.csv, fread}
}
\value{
the loeaded data.table or frame
}
\description{
Finds the file that contains any number of strings at a specified location and loads as a data.frame or table
}
