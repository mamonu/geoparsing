#simple script to run a csv through GEOLocate webservices
#returns two files. One with all the results and another with only the first result of each run
#for this to work csv will need at least the following column headers in any order but case sensitive: Country,Locality,StateProvince,County
#Question & comments to Nelson Rios(nrios@tulane.edu)
#last modified: 05 Oct 2012 

library(RJSONIO)
library(RCurl)

setwd("/home/bigdata/scripts/geoparsing")

INPUTFILENAME="glc_in.csv"
OUTPUTFILENAME="glc_out.csv"
OUTPUTFILENAMEFIRSTRESULT="glc_out_first_result.csv"


OPTIONS="&doduncert=true&dopoly=false&displacepoly=false"
glcIn= read.csv(INPUTFILENAME)
numGLCRuns = 0
recordCounter = 0
for (k in 1:nrow(glcIn)){
  print(k)
	Sys.sleep(3) #be nice and pause a few seconds between requests	
	Country=glcIn[k,]$Country
	Locality=glcIn[k,]$Locality
	StateProvince=glcIn[k,]$StateProvince
	County=glcIn[k,]$County
	q=paste("http://www.museum.tulane.edu/webservices/geolocatesvcv2/glcwrap.aspx?country=",Country,"&locality=",Locality,"&state=",StateProvince,"&county=",County,OPTIONS, sep='')
	q=gsub(' ','%20',q)
	
  tryCatch({
    JSONresponse = basicTextGatherer()
    curlPerform(url = q, writefunction = JSONresponse$update)
    glcRecNum = k
    glc = fromJSON(JSONresponse$value())
    numresults = glc$numResults
    if (numresults > 0){ 
      for (i in 1:numresults) {
        glcRank  = i
        glcLongitude = glc$resultSet$features[[i]]$geometry$coordinates[1]
        glcLatitude = glc$resultSet$features[[i]]$geometry$coordinates[2]
        glcPrecision = glc$resultSet$features[[i]]$properties$precision
        glcScore = glc$resultSet$features[[i]]$properties$score
        glcParsepattern = glc$resultSet$features[[i]]$properties$parsePattern
        glcUncert = glc$resultSet$features[[i]]$properties$uncertaintyRadiusMeters
        glcPoly = glc$resultSet$features[[i]]$properties$uncertaintyPolygon
        #if a polygon is present reformat coordinates to geolocate format-a comma delimited array
        if ("coordinates"%in%names(glcPoly)){
          sPoly = ''
          for (v in 1:length(glcPoly$coordinates[[1]][])){
            vLon=format(glcPoly$coordinates[[1]][[v]][1])
            vLat=format(glcPoly$coordinates[[1]][[v]][2])
            sPoly  = paste(sPoly,vLat, vLon, sep=',')
          }
          # Strip the leading commas
          sPoly=sub("^,+", "", sPoly)
          glcPoly=sPoly
        }
        df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
        recordCounter = recordCounter + 1
        if (recordCounter==1)
          write.table(x=df, file=OUTPUTFILENAME, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',', qmethod="double") else
            write.table(x=df, file=OUTPUTFILENAME, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',', qmethod="double")
      }
    } else {
      glcRank  = 1
      glcLongitude = NA
      glcLatitude = NA
      glcPrecision = NA
      glcScore = NA
      glcParsepattern = NA
      glcUncert = NA
      glcPoly = NA
      df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
      recordCounter = recordCounter + 1
      if (recordCounter==1)
        write.table(x=df, file=OUTPUTFILENAME, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',') else
          write.table(x=df, file=OUTPUTFILENAME, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',')	
    }
  },error = function(err) 
  {
    glcRank  = 0
    glcLongitude = NA
    glcLatitude = NA
    glcPrecision = "ERROR GETTING JSON"
    glcScore = 0
    glcParsepattern = NA
    glcUncert = NA
    glcPoly = NA
    df = data.frame(glcRecNum,glcRank,glcLatitude,glcLongitude,glcPrecision,glcScore,glcParsepattern,glcUncert,glcPoly,replace(glcIn[k,], is.na(glcIn[k,]),""))
    recordCounter = recordCounter + 1
    if (recordCounter==1)
      write.table(x=df, file=OUTPUTFILENAME, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',') else
        write.table(x=df, file=OUTPUTFILENAME, append=TRUE,  row.names=FALSE, col.names=FALSE, quote=TRUE, sep=',')  
    
  })
}

glcFiltered=read.csv(OUTPUTFILENAME)
glcFiltered=glcFiltered[glcFiltered$glcRank==1,]
write.table(x=glcFiltered, file=OUTPUTFILENAMEFIRSTRESULT, append=FALSE, row.names=FALSE, col.names=TRUE,  quote=TRUE, sep=',')

#freeing resources
rm(list=ls())


