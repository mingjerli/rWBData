\name{search.catalogs}
\alias{search.catalogs}
\title{
Search available data catalogs
}
\description{
This function will search available data catalog (name and acronym) using user provided text.
}
\usage{
search.catalogs(sText, simple = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sText}{a character represents text to search}
  \item{simple}{\code{TRUE/FALSE}; TRUE for full description of data catalog, FALSE for simplified description of data catalog}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function always returns a data frame the following three columns.
\item{ID}{ID of the data catalog}
\item{name}{name of the data catalog}
\item{acronym}{acronym of the data catalog}
If \code{simple=FALSE}, the returned data will include the following extra 23 columns
\item{dexcription}{description of the data catalog}
\item{url}{url of the data catalog}
\item{type}{type of the data catalog}                                   
\item{languagesupported}{available language of the data catalog}                       
\item{periodicity}{data periodicity of the data catalog}                            
\item{economycoverage}{which economy the data catalog covers}                         
\item{granularity}{granularity of the data catalog}                            
\item{numberofeconomies}{number of economics the data catalog covers}                       
\item{topics}{topics of the data catalog}                                 
\item{updatefrequency}{update frequency of the data catalog}                         
\item{updateschedule}{update schedule of the data catalog}                         
\item{lastrevisiondate}{last update of the data catalog}                        
\item{contactdetails}{contact information of the data catalog}                         
\item{accessoption}{options to access the data catalog}                            
\item{bulkdownload}{bulk download is available or not for the data catalog}                           
\item{cite}{citation of the data catalog}                                    
\item{detailpageurl}{detail page url of the data catalog}                          
\item{coverage}{coverage of the data catalog}                                
\item{api}{api available of the data catalog}                                    
\item{apiaccessurl}{api access url of the data catalog}                           
\item{apisourceid}{api source id of the data catalog}                            
\item{mobileapp}{mobile app available of the data catalog}                               
\item{datanotes}{note of the data catalog}                              
\item{listofcountriesregionssubnationaladmins}{list of countries, regions, subnation, admins} 
\item{sourceurl}{source url of the data catalog} 
If there is no result matches, it will returns a data frame with zero row.
}
\author{
Ming-Jer Lee <mingjerli@gmail.com>
}
\seealso{
\code{available.catalogs}
}
\examples{
search.catalogs("GDP")[1:3,]
search.catalogs("GDP", simple=TRUE)[1:5,]
}

