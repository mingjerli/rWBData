\name{get.indicator}
\alias{get.indicator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the query result from a data indicator
}
\description{
This function returns the query result of a data indicator as a data frame.
}
\usage{
get.indicator(country, indicator, per_page = 100, from = 1960, to = 2014)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{country}{A character represents the id or iso2code of the country}
  \item{indicator}{A character represents the id of the data indicator}
  \item{per_page}{Maximum number of observed data}
  \item{from}{Starting year}
  \item{to}{End year}
}
\value{
If the data is available, this function will return a data frame; otherwise, NA.
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\examples{
countries <- unlist(categoryList$country[,"id"])
indicators <- unlist(categoryList$indicator[,"id"])
get.indicator(countries[119], indicators[68], 10, from=2010, to=2014)
get.indicator("USA","1.2_ACCESS.ELECTRICITY.RURAL")
}
