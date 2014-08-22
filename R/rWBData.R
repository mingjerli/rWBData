load("data/categoryList.Rdata")

updateCategoryList <- function(){
  
  getList <- function(listName, per_page=1000){
    url = paste("http://api.worldbank.org/", listName,"?per_page=",per_page,"&format=json", sep="")
    dataList = RJSONIO::fromJSON(url, nullValue=NA)[[2]]
    dataList = plyr::llply(dataList, function(j) t(cbind(j[])))
    dataList = data.frame(do.call('rbind', dataList))
  }
  
  getDataCatalog <- function(){
    per_page=300
    url = paste("http://api.worldbank.org/v2/datacatalog?per_page=",per_page,"&format=json", sep="")
    dataList = RJSONIO::fromJSON(url, nullValue=NA)[[5]]
    dataID = plyr::ldply(dataList, function(j) j[[1]])
    dataList = plyr::llply(dataList, function(j) j[[2]])
    dataList = plyr::llply(dataList, function(j) t(matrix(do.call('c',j), nrow=2)))
    dataList = plyr::llply(dataList, function(j){l <- as.list(j[,2]) ; names(l)<-j[,1]; l})
    dataList = plyr::ldply(dataList, data.frame)
    dataList = cbind(dataID, dataList)
    dimnames(dataList)[[2]][1] = "ID"
    dataList
  }
  message("Be patient! This function takes minutes to run!")
  countryList = getList(listName="country")
  regionList = getList(listName="region")
  lendingtypeList = getList(listName="lendingtype")
  incomelevelList = getList(listName="incomelevel")
  sourceList = getList(listName="source")
  indicatorList = getList(listName="indicator", per_page=10000)
  topicList = getList(listName="topic")
  dataCatalogList = getDataCatalog()
  categoryList = list(country = countryList, 
                      region = regionList,
                      lendingtype = lendingtypeList,
                      incomelevel = incomelevelList,
                      datasource = sourceList,
                      indicator = indicatorList,
                      topic = topicList,
                      datacatalog = dataCatalogList)
  save(categoryList , file="data/categoryList.Rdata")
  load("data/categoryList.Rdata")
}

searchWorldBankData <- function(sText, cList="all"){
  catList <- eval(parse(text=paste("categoryList$",cList, sep="")))
  dfHit <- apply(catList, c(1,2), function(x) grepl(sText, x, ignore.case=TRUE))
  dfHit <- apply(dfHit, 1, any)
  catList[dfHit,]
}

available.catalogs <- function(){
  categoryList$datacatalog[ ,1:3]
}

available.indicators <- function(){
  categoryList$indicator[ ,1:2]
}

search.catalogs <- function(sText, simple=FALSE){
  result <- searchWorldBankData(sText, "datacatalog")
  if(simple){
    result <- result[,1:3]
  }
  result
}

search.indicators <- function(sText, simple=FALSE){
  result <- searchWorldBankData(sText, "indicator")
  if(simple){
    result <- result[,1:2]
  }
  result
}

# return a data frame or NULL
get.catalog  <- function(dIndex){
  df <- NULL
  apiStatus <- categoryList$datacatalog[dIndex,"api"]
  if(!is.na(apiStatus) && apiStatus==1){
    dIndexApiUrl <- categoryList$datacatalog[dIndex,"apiaccessurl"]
    if(dIndexApiUrl == "http://data.worldbank.org/developers/world-bank-finances"){
      df <- RSocrata::read.socrata(categoryList$datacatalog[dIndex,"url"])
    }
    if( dIndexApiUrl== "http://search.worldbank.org/api/v2/projects"){
      message("This data catalog has not been support yet. Please go to http://search.worldbank.org/api/v2/projects")
      return(NA)
    }
    if( dIndexApiUrl== "http://search.worldbank.org/api/v2/wds"){
      message("This data catalog has not been support yet. Please go to http://search.worldbank.org/api/v2/wds")
      return(NA)      
    }
    if( dIndexApiUrl== "http://data.worldbank.org/developers/climate-data-api"){
      message("Please use rWBclimate packages")
      return(NA)      
    }
    if( dIndexApiUrl== "http://data.worldbank.org/developers"){
      message("Please use get.indicator() function")
      return(NA)
    }
  }
  else{
    message(paste("'",categoryList$datacatalog[dIndex,"name"],"'"," does not support API access.",sep=""))
    return(NA)
  }
  df
}

# return a data frame
get.indicator <- function(country, indicator, per_page=100, from=1960, to=2014){
  country <- as.character(country)
  indicator <- as.character(indicator)
  if(!is.element(country, categoryList$country$id) && !is.element(country, categoryList$country$iso2code)){
    message(paste("'",country,"'"," is not a proper code for a country.",sep=""))
    return(NA)
  }
  if(!is.element(indicator, categoryList$indicator$id)){
    message(paste("'",indicator,"'"," is not a proper id for a indicator",sep=""))
    return(NA)
  }
  
  url <- paste("http://api.worldbank.org/countries/", country,
               "/indicators/", indicator, 
               "?per_page=", per_page, "&date=",from,":",to , 
               "&format=json", sep="")
  rawData <- RJSONIO::fromJSON(url, nullValue=NA)[[2]]
  if(is.na(rawData) && length(rawData)<2){
    return(NA)
  }
  plyr::ldply(rawData,unlist)  
}

# save the data catalog
download.catalog <-  function(dIndex, saveFile=FALSE, type="all"){
  fileStatus <- categoryList$datacatalog[dIndex,"bulkdownload"]
  filePath <- unlist(strsplit(as.character(fileStatus), split=" "))
  filePath <- filePath[grepl(paste("http",sep=""),filePath)]
  filePath <- unlist(strsplit(as.character(filePath), split=";"))
  filePath <- filePath[grepl(paste("http",sep=""),filePath)]
  filePath <- unlist(strsplit(as.character(filePath), split="="))  
  filePath <- filePath[grepl(paste("http",sep=""),filePath)]
  
  csvfilePath <- filePath[grepl("csv", filePath)]
  xlsfilePath <- filePath[unique(c(grepl("xls", filePath),grepl("excel", filePath)))]
  pdffilePath <- filePath[grepl("pdf", filePath)]
  zipfilePath <- filePath[grepl("zip", filePath)]
  xmlfilePath <- filePath[grepl("xml", filePath)]
  jsonfilePath <- filePath[grepl("json", filePath)]

  goodFormat <- c("csv","excel","xls","pdf","zip","xml","json","all")
  if(is.element(type,goodFormat)){
    if(type=="csv"){
      filePath <- csvfilePath
    }
    if(type=="excel" || type=="xls"){
      filePath <- xlsfilePath
    }
    if(type=="pdf"){
      filePath <- pdffilePath
    }
    if(type=="zip"){
      filePath <- zipfilePath
    }
    if(type=="xml"){
      filePath <- xmlfilePath
    }
    if(type=="json"){
      filePath <- jsonfilePath
    }
    if(type=="all"){
      filePath <- unique(c(csvfilePath, xlsfilePath, pdffilePath, zipfilePath, xmlfilePath, jsonfilePath))
    }
  }
  else{
    message(paste(type, " format does not support bulk download."))
    return(NULL)
  }
  if(length(filePath)==0 || is.null(filePath)){
    message(paste("'",as.character(categoryList$datacatalog[dIndex,"name"]),"'"," does not support bulk download.",sep=""))
    return(NULL)
  }
  
  if(saveFile){
    for(fP in filePath){
      fileName <- unlist(strsplit(fP,split="/"))
      fileName <- fileName[length(fileName)]
      download.file(filePath, fileName)
    }
  }
  filePath
}
