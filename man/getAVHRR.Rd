\name{getAVHRR}

\alias{getAVHRR}

\title{Find and download AVHRR NDVI Climate Data Record (CDR)}

\description{
  Find and download AVHRR NDVI CDR for a time period. 
  The function returned is list of filenames that may have been downloaded.
}

\usage{
  getAVHRR(start_date, end_date, path = "", overwrite = FALSE, update = FALSE, ...)
}

\arguments{
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character. end date for the data requested formatted yyyy-m-d}
  \item{overwrite}{logical. If \code{TRUE}, existing data will be overwritten}
  \item{path}{character. Path name indicating where to store the data}
  \item{update}{logical. If \code{TRUE}, existing records of AVHRR NDVI CDR will be updated}
  \item{...}{Additional arguments that can be passed to \code{download.file})}
}


\value{
  character vector of file names pointing to the downloaded files
}

\details{
  Files may not be available during server downtime. 
}

\examples{  
  sdate <- "2019-05-17" 
  edate <- "2019-05-23"
  dir <- tempdir()
  ff <- getAVHRR(start_date, end_date, dir)
}

