library(rjson)
library(rvest)
library(stringr)
library(parallel)

#Create list of dates to query

dates<-as.character(as.Date(as.Date("2010-11-15"):as.Date("2017-12-31"), origin="1970-01-01"))
dates2<-gsub("-", "", dates)

#Fetch Wayback URLs

jsonfn<-function(i){
  file<-fromJSON(file=paste("https://archive.org/wayback/available?url=http://itunes.apple.com/us/app/path/id403639508?mt=8&timestamp=", dates2[i], sep=""))
  return(file$archived_snapshots$closest$url)
}
waybacks<-unlist(lapply(1:length(dates2), jsonfn))

waybacks<-unique(waybacks)

datefn<-function(i){
  year<-str_sub(waybacks[i], 28, 31)
  month<-str_sub(waybacks[i], 32, 33)
  day<-str_sub(waybacks[i], 34, 35)
  date<-paste(year, "-", month, "-", day, sep="")
  return(date)
}

dates<-unlist(mclapply(1:length(waybacks), datefn, mc.cores=detectCores()-1))

timefn<-function(i){
  hours<-str_sub(waybacks[i], 36, 37)
  minutes<-str_sub(waybacks[i], 38, 39)
  seconds<-str_sub(waybacks[i], 40, 41)
  time<-paste(hours, ":", minutes, ":", seconds, sep="")
  return(time)
}

times<-unlist(mclapply(1:length(waybacks), timefn, mc.cores=detectCores()-1))

wayback<-as.data.frame(cbind(dates, times, waybacks))

names(wayback)<-c("Date", "Time", "URL")
wayback$URL<-as.character(wayback$URL)

#Fetch contents of description

descfn<-function(i){
  webpage<-read_html(wayback$URL[i])
  rank_data_html<-html_nodes(webpage,'.product-review')
  outs<-html_text(rank_data_html)
  if(length(outs)>1){
    temp<-outs[1]
    for(j in 2:length(outs)){
      temp<-paste(temp, outs[j])
    }
    outs<-temp
  }
  return(outs)
}

wayback$Description<-lapply(1:dim(wayback)[1], descfn)

setwd("/Users/Matthew/Box/Project Path/Data/Raw Wayback Data/Full Scraped Data/")
openxlsx::write.xlsx(wayback, "Wayback_Complete.xlsx")


for(i in 1:length(totest)){
  testing<-unlist(wayback$Description[wayback$Date==totest[i]])
  if(testing[1]==testing[2]){
    print(paste(i, "TRUE"))
  }
  if(testing[1]!=testing[2]){
    print(paste(i, "FALSE"))
  }
}
