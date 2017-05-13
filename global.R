library(leaflet)
library(shiny)
library(ggplot2)
library(plotly)

#Dates = c("0909","0910","0911","0912","0913","0914")
Dates = c("1201","1202","1203")
CriteriaCols = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C10", "CX1")
CriteriaSort = c(0,0,1,1,0,1,0,1,1,0) # decides how to sort the output table


if (FALSE){ # Calcuate Intermediate Data Tables and save as rda file


# ByCriteriaData ----------------------------------------------------------
CombinedDataByCriteria = data.frame()
for (i in 1:length(Dates)){
  data<-read.csv(file=paste("../OutputTable/",Dates[i],"ByCriteria.csv",sep=""),sep=",");
  CombinedDataByCriteria <- rbind(CombinedDataByCriteria, cbind(data, rep(Dates[i],nrow(data))))
}
colnames(CombinedDataByCriteria)[c((ncol(CombinedDataByCriteria)-2) : ncol(CombinedDataByCriteria))] = c("flag", "total", "date")
save(CombinedDataByCriteria, file="./rda/CombinedDataByCriteria.rda");

# ByTimeData ----------------------------------------------------------
CombinedDataByTime = data.frame()
for (i in 1:length(Dates)){
  data<-read.csv(file=paste("../OutputTable/",Dates[i],"ByTime.csv",sep=""),sep=",");
  CombinedDataByTime <- rbind(CombinedDataByTime, cbind(data, rep(Dates[i],nrow(data))))
}
colnames(CombinedDataByCriteria)[c((ncol(CombinedDataByCriteria)-2) : ncol(CombinedDataByCriteria))] = c("flag", "total", "date")
colnames(CombinedDataByTime)[ncol(CombinedDataByTime)] = "date"

AggDataByTime= data.frame(hr = numeric(0), flag_per = numeric(0), date=character())
for (i in 1:length(Dates)){
  temp_data = subset(CombinedDataByTime, date == Dates[i])
  temp_data[is.na(temp_data)] <- 0
  for (j in 1:24){
    newrow = data.frame(hr = j, flagged = sum(temp_data[,j+1]) / sum(temp_data[,j+26]), date = Dates[i])
    AggDataByTime = rbind(AggDataByTime, newrow)
  }
}

save(AggDataByTime, file="./rda/AggDataByTime.rda");

# ByDetectorData ----------------------------------------------------------

AllDetectorData =  data.frame()
for (i in 1:length(Dates)){
  data<-read.csv(file=paste("../OutputTable/",Dates[i],"FinalDF.csv",sep=""),sep=",");
  save(data, file=paste("./rda/ByDetectorData_",Dates[i],".rda",sep=""))
  data<-cbind(data, rep(1, nrow(data)))
  colnames(data)[ncol(data)] = "count"
  date_time = as.POSIXct(paste("2015", as.character(Dates[i])," 00:00:00",sep=""), format = "%Y%m%d %H:%M:%S") #create time
  #data<-ExcludeNumberOfLane(data)    ### for test purpose on Dec5, 2016
  data<-aggregate(data[,c("flag","count",CriteriaCols)], by = list(detector_id = data$detector_id, timestamp = data$timestamp,hour = data$hour), FUN = sum)
  data$flag_whole_detector = as.numeric(data$flag>0)
  data<-aggregate(data[,c("flag","flag_whole_detector","count",CriteriaCols)], by = list(detector_id = data$detector_id, hour = data$hour), FUN = sum)
  DateTime = rep(date_time, nrow(data))
  for (j in 1:nrow(data)) DateTime[j] = date_time + data[j,"hour"] * 3600  #add hours to time
  AllDetectorData <- rbind(AllDetectorData, cbind(data, DateTime))
}

AllDetectorData = subset(AllDetectorData, detector_id %in% UniqueValidDetectors)
save(AllDetectorData, file="./rda/AllDetectorData.rda");

# ByLocationData ----------------------------------------------------------
data<-read.csv(file="../LocationData/DetectorLoc.csv",sep=",");
data_filter = na.omit(data)
DetectorCenter<-aggregate(data_filter[,c("MidLon","MidLat")], by = list(data_filter$DetectorID), FUN = mean)
colnames(DetectorCenter)[1] = "DetectorID"
DetectorCenter<-merge(x = DetectorCenter, y = unique(data_filter[,c("DetectorID","DetectorName")]), by = "DetectorID", all.x = TRUE)
DetectorCenter$MidLon = DetectorCenter$MidLon / 1000000
DetectorCenter$MidLat = DetectorCenter$MidLat / 1000000

LinkCenter<-aggregate(data_filter[,c("MidLon","MidLat")], by = list(data_filter$DetectorID,data_filter$LinkName), FUN = mean)
colnames(LinkCenter)[c(1,2)] = c("DetectorID","LinkName")
LinkCenter$MidLon = LinkCenter$MidLon / 1000000
LinkCenter$MidLat = LinkCenter$MidLat / 1000000

save(DetectorCenter, file="./rda/DetectorCenter.rda");
save(LinkCenter, file="./rda/LinkCenter.rda");

} # end if FALSE

# Load Variables from rds files ----------------------------------------------------------

load(file="./rda/CombinedDataByCriteria.rda")
load(file="./rda/AggDataByTime.rda")
load(file="./rda/AllDetectorData.rda")
load(file="./rda/DetectorCenter.rda")
load(file="./rda/LinkCenter.rda")
load(file="./rda/config.rda")
#for (i in 1:length(Dates)) assign(paste("ByDetectorData",Dates[i],sep=""), get(load(file=paste("ByDetectorData_",Dates[i],".rda", sep=""))))

# BL Report Table

AllDetectorData_agg<-aggregate(AllDetectorData[,c("flag","count",CriteriaCols)], by = list(detector_id = AllDetectorData$detector_id), FUN = sum)
for (i in 4:13){AllDetectorData_agg[,i] = AllDetectorData_agg[,i] / AllDetectorData_agg[,"count"]}
AllDetectorData_agg[,"flag_percent"] = AllDetectorData_agg[,"flag"] / AllDetectorData_agg[,"count"]

# Others ----------------------------------------------------------

#config<-read.csv(file="../Configuration/configuration.csv",sep=",");
#save(config, file="./rda/config.rda");
ValidDetectors = config$detector_id[which(config$BM==0)]
UniqueDetectorIDs = unique(AllDetectorData$detector_id)
UniqueDateTimes = unique(AllDetectorData$DateTime)
minDatetime = min(UniqueDateTimes)
maxDatetime = max(UniqueDateTimes)
UniqueValidDetectors = UniqueDetectorIDs[UniqueDetectorIDs %in% ValidDetectors]

#Exclude Rule 10, Only for testing purpose

ExcludeNumberOfLane<-function(Data){
CriteriaCols_exclude = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "CX1")
Data[["flag"]] = as.numeric(rowSums(Data[,CriteriaCols_exclude])>0)
return (Data)
}

#Graphics
compute_bins <- function(x, size) {
  list(
    start = min(x),
    end = 10000,
    #end = quantile(x,0.95),
    size = size
  )
}
m2 <- list(color = toRGB("black", 0.2))