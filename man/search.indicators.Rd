\name{search.indicators}
\alias{search.indicators}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search available data indicators
}
\description{
This function will search available data indicators (ID and name) using user provided text.
}
\usage{
search.indicators(sText, simple = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sText}{a character represents text to search}
  \item{simple}{\code{TRUE/FALSE}; TRUE for full description of data indicator, FALSE for simplified description of data indicator}
}
\value{
This function always returns a data frame the following three columns.
\item{id}{ID of the data indicator}
\item{name}{name of the data indicator}
If \code{simple=FALSE}, the returned data will include the following extra 23 columns
\item{source}{source of the data indicator}
\item{sourceNote}{source note of the data indicator}
\item{sourceOrganization}{source organization of the data indicator}                                   
\item{topics}{topics of the data indicator}        
If there is no result matches, it will returns a data frame with zero row.
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\examples{
search.indicators("GDP")[1:3,]
search.indicators("GDP", simple=TRUE)[1:3,]
}