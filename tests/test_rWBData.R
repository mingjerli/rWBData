
test.download.catalog <- function(){
  for(i in 1:164){
    print(i)
    print(download.catalog(i))
    print(download.catalog(i,type="csv"))
    print(download.catalog(i,type="xls"))
    print(download.catalog(i,type="hiv"))
  }
}

test.get.indicator <- function(){
  countries <- unlist(categoryList$country[,"id"])[249]
  indicators <- unlist(categoryList$indicator[,"id"])
  availableMat <- matrix(data=NA, nrow=length(countries), ncol=length(indicators))
  for(cIndex in 1:length(countries)){
    for(iIndex in 1:length(indicators)){
      rawResult <- get.indicator(countries[cIndex], indicators[iIndex], 10, from=2010, to=2014)
      availableMat[cIndex, iIndex] = !(length(rawResult)==1 && is.na(rawResult))
      print(paste(cIndex, iIndex))
      print(head(rawResult))
    }
  }
  availableMat[1,1:100]
}

test.get.catalog <- function(from=1, to=NULL){
  if(is.null(to)){
    to = dim(categoryList$datacatalog)[1]
  }
  for(i in from:to){
    print(paste(i,categoryList$datacatalog[i,"name"]))
    get.catalog(i)
  }
}