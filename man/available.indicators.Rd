\name{available.indicators}
\alias{available.indicators}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get available data indicators
}
\description{
This function will return all available data indicators listed in world bank open data.
}
\usage{
available.indicators()
}
\value{
This function returns a data frame with three columns.
\item{ID}{ID of the data indicator}
\item{name}{name of the data indicator}
}
\references{
http://data.worldbank.org/indicator
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\examples{
available.indicators()
available.indicators()[1:5,]
}
