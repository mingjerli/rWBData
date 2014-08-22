\name{get.catalog}
\alias{get.catalog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a data catalog as a data frame.
}
\description{
This function will download the whole data catalog(if available) and save it as a data frame.
}
\usage{
get.catalog(dIndex)
}
\arguments{
  \item{dIndex}{A numerical value to represent catalog ID, see \code{available.catalogs()}}
}
\value{
This function will return a data frame if the data catalog is available, otherwise it will show how to access the data catalog and return NA.
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\examples{
get.catalog(135)
get.catalog(1)
}

