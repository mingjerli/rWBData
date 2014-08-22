\name{available.catalogs}
\alias{available.catalogs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get available data catalog
}
\description{
This function will return all available data catalog listed in world bank open data.
}
\usage{
available.catalogs()
}
\value{
This function returns a data frame with three columns.
\item{ID}{ID of the data catalog}
\item{name}{name of the data catalog}
\item{acronym}{acronym of the data catalog} 
}
\references{
http://datacatalog.worldbank.org/
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\examples{
available.catalogs()
available.catalogs()[1:5,]
}
