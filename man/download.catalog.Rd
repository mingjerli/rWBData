\name{download.catalog}
\alias{download.catalog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bulk download the whole data catalog
}
\description{
This func
}
\usage{
download.catalog(dIndex, saveFile = FALSE, type = "all")
}
\arguments{
  \item{dIndex}{A numerical value to represent catalog ID, see \code{available.catalogs()}. }
  \item{saveFile}{\code{TRUE/FALSE}; TRUE means this function will automatically download the file and return the file url, FALSE means this function will only return the file url.}
  \item{type}{A character to specify which type of file to be download if available. Currently support "csv, "xls"(or "excel"), "pdf", "zip", "xml, and "json". Choose "all" to download all possible file (may get same result in different format).
  }
}
\value{
This function will return a (vector of) character to represent the url(s) of the data catalog. 
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\examples{
download.catalog(15)
download.catalog(20,type="csv")
}
