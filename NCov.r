# In Progress on this cruft
# No tidyverse or ggplot here!
# 9:29 AM 5/6/2020 -RMF
require("NCoVUtils")
require("data.table")
library(lubridate)
library(lattice)
fsum <- function(x) {sum(x,na.rm=TRUE)}
setwd("D:/Politics/CovD2019")
reset_cache()

names(as.data.table(NCoVUtils::get_linelist()))
ll <- as.data.table(NCoVUtils::get_linelist())
ll[,.N,.(country)][order(-N)][1:100]
ll[country=="China",.(country,admin1),keyby="outcome"][,.N,.(outcome,admin1)][order(-N)]
ll[country=="United States",.(country,admin1),keyby="outcome"][,.N,.(outcome,admin1)][order(-N)]
get_us_regional_cases(level = "county", out = "timeseries")
as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",dcast(.SD,county ~ NULL,value.var=c("cases","deaths"),fun.aggregate=fsum)]


reset_cache()
lr <- as.data.table(get_us_regional_cases(level = "state", out = "timeseries"))[,.(state,deaths,date=date)][deaths != 0,]
lr[,cumsum:=cumsum(deaths)]
dev.new();lr[,plot(cumsum ~ as.factor(ymd(as.Date(date))),las=2)]

lrs_ <- {}
as.data.table(get_us_regional_cases(level = "state", out = "timeseries"))[,.(state,deaths,date=date)][deaths != 0,]
lr_s <- lr
for(i in sort(unique(lr_s$state))) {
lrs <- lr_s[state == i,];
lrs[,cumsum:=cumsum(deaths)];
lrs_ <- rbind(lrs_,lrs)}
lrs_[,.(deaths=max(cumsum)),.(state)][order(-deaths)][1:25]


StatesPop <- fread("nst-est2019-07.csv")
sdeathp <- merge(StatesPop,lrs_[,.(deaths=max(cumsum)),.(state)],by="state")
sdeathp[,dp100K:=round((deaths/population) * 10^5,2)]
sdeathp[,dp1M:=round((deaths/population) * 10^6,2)]
sdeathp[]


#working but gives wrong data?
cat('
dev.new()
lrs_[state %in% StatesPop[order(-population)]$state,][
,barchart(date ~ cumsum,data=.SD,origin=0,scales=list(cex=c(.75,1)),
xlab = "Deaths: Data from https://github.com/nytimes/covid-19-data by way of https://github.com/epiforecasts/NCoVUtils",
main = "States with 'New York',barchart(date ~ cumsum)")]
#
dev.new()
lrs_[state %in% StatesPop[order(-population)]$state,][
state != "New York",barchart(date ~ cumsum,data=.SD,origin=0,scales=list(cex=c(.75,1)),
xlab = "Deaths: Data from https://github.com/nytimes/covid-19-data by way of https://github.com/epiforecasts/NCoVUtils",
main = "States without 'New York',barchart(date ~ cumsum)")]
#

dev.new()
lrs_[state %in% StatesPop[order(-population)][1:10]$state,][
state != "New York",barchart(date ~ cumsum,data=.SD,origin=0,scales=list(cex=c(.75,1)),
xlab = "Deaths: Data from https://github.com/nytimes/covid-19-data by way of https://github.com/epiforecasts/NCoVUtils",
main = "Top Ten state population without 'New York',barchart(date ~ cumsum)")]
#

dev.new()
lrs_[state %in% StatesPop[order(-population)][1:10]$state,][,barchart(date ~ cumsum,data=.SD,origin=0,scales=list(cex=c(.75,1)),
xlab = "Deaths: Data from https://github.com/nytimes/covid-19-data by way of https://github.com/epiforecasts/NCoVUtils",
main = "Top Ten state population: barchart(date ~ cumsum)")]
# ')


dev.new();
lrs_[,barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.5,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][1:10]$state,][,barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.5,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][1:10]$state,][state == "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][1:10]$state,][state != "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.5,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][1:6]$state,][state != "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][1:5]$state,][state != "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][7:11]$state,][state != "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][12:51]$state,][state != "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)][12:18]$state,][state != "New York",barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]

dev.new()
lrs_[state %in% StatesPop[order(-population)]$state,][state %in% c("Texas","California","New York"),barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]
dev.new()
lrs_[state %in% StatesPop[order(-population)]$state,][state %in% c("Texas","California","New York","Florida"),barchart(date ~ cumsum | state,data=.SD,origin=0,scales=list(cex=c(.75,1)))]



lr_wa <- lr[state == "Washington",] 
lr_wa[,cumsum:=cumsum(deaths)]
dev.new();lr_wa[,plot(cumsum ~ as.factor(ymd(as.Date(date))),las=2)]

lr_ny <- lr[state == "New York",] 
lr_ny[,cumsum:=cumsum(deaths)]
dev.new();lr_ny[,plot(cumsum ~ as.factor(ymd(as.Date(date))),las=2)]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(cases,date=date),keyby="county"][cases != 0,barchart(date ~ cases | county,origin=0,cex=.5)]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(cases,date=date),keyby="county"][cases != 0,barchart(date ~ cases | county,origin=0,cex=.25)]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(cases,date=date),keyby="county"][cases != 0 & county %in% c("King","Pierce","Snohomish","Clark","Spokane","Whatcom","Yakima"),barchart(date ~ cases | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(cases,date=date),keyby="county"][cases != 0 & county %in% c("King","Pierce","Snohomish","Clark","Spokane","Benton","Whatcom","Yakima"),barchart(date ~ cases | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("King","Pierce","Snohomish","Clark","Spokane","Benton","Whatcom","Yakima"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("King","Pierce","Snohomish","Clark","Spokane","Thurston","Kitsap","Benton","Whatcom","Yakima"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("Pierce","Snohomish","Clark","Spokane","Thurston","Kitsap","Benton","Whatcom","Yakima"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("Pierce","Clark","Spokane","Thurston","Kitsap","Benton","Whatcom","Yakima"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("King","Snohomish"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.5,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("King","Snohomish"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.75,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("Whatcom","Yakima","Benton"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.75,1)))]

as.data.table(get_us_regional_cases(level = "county", out = "timeseries"))[state == "Washington",.(deaths,date=date),keyby="county"][deaths != 0 & county %in% c("Whatcom","Skagit","Yakima","Benton"),barchart(date ~ deaths | county,data=.SD,origin=0,scales=list(cex=c(.75,1)))]> 



