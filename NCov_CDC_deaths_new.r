# Original Data from https://www.cdc.gov/nchs/nvss/vsrr/covid19/index.htm
# New Data from https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku
# Covid19: Death vs Age 5/01/2020

library(data.table)
library(lattice)
library(Hmisc)

ByAge <- fread("Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State_05.01.2020.csv",encoding="UTF-8")
names(ByAge) <- ByAge[,gsub("-","_",colnames(.SD))]
names(ByAge) <- ByAge[,gsub(" ","",colnames(.SD))]
names(ByAge) <- ByAge[,gsub(",","",colnames(.SD))]
ByAge_ <- ByAge[,c(5,6,7,9,8)][Sex=="All Sexes",]
ByAge__ <- setnames(ByAge_[,c(2:5)],c("Age.Group","Covid19.Deaths","Pneum.Deaths","All.Deaths"))[]

ByAgeChart <- ByAge__
ByAgeChart[,Age.Index := .I]
setcolorder(ByAgeChart,c("Age.Index","Age.Group","Covid19.Deaths","Pneum.Deaths","All.Deaths"))
ByAgeChart[1,2] <- c("1.year.or.under")
ByAgeChart[11,2] <- c("85.years.and.older")

dev.new()
ByAgeChart[,barchart(Covid19.Deaths + Pneum.Deaths ~ as.factor(Age.Index),allow.multiple=TRUE,origin=0)]

#not working
dev.new()
ByAgeChart[,barchart(Covid19.Deaths + Pneum.Deaths ~ as.factor(Age.Group),allow.multiple=TRUE,origin=0)]



ByAgeChart[,cor(Covid19.Deaths,Pneum.Deaths)]
ByAgeChart[,cor(Covid19.Deaths,All.Deaths)]
ByAgeChart[,cor(Pneum.Deaths,All.Deaths)]
ByAgeChart[,cor(cbind(Covid19.Deaths,Pneum.Deaths,All.Deaths))]
ByAgeChart[,rcorr(cbind(Covid19.Deaths,Pneum.Deaths,All.Deaths))]
ByAgeChart[]

dev.new()
par(mfrow=c(3,1))
ByAgeChart[,plot(density(All.Deaths),cex=2,lwd=2,col="black")]
ByAgeChart[,plot(density(Pneum.Deaths),cex=2,lwd=2,col="red")]
ByAgeChart[,plot(density(Covid19.Deaths),cex=2,lwd=2,col="blue")]
par(mfrow=c(1,1))

dev.new()
ByAgeChart[,plot(density(All.Deaths),xlab="",ylab="",xaxt="n",yaxt="n",main="",cex=2,lwd=3,col="black")]
par(new=TRUE)
ByAgeChart[,plot(density(Pneum.Deaths),xlab="",ylab="",xaxt="n",yaxt="n",main="",cex=2,lwd=3, col="red")]
par(new=TRUE)
ByAgeChart[,plot(density(Covid19.Deaths),xlab="Same Scaled Bandwidth",ylab="Same Scaled Density",xaxt="n",yaxt="n",
main="Density Plots: Black=All Deaths Red=Pneumonia Deaths Blue=Covid=19 Deaths",cex=2,lwd=3,col="blue")]
par(new=FALSE)

dev.new()
par(mfrow=c(3,1))
ByAgeChart[,plot(All.Deaths ~ (Age.Index),cex=2,lwd=2,pch=19,type="b",col="black")]
ByAgeChart[,plot(Pneum.Deaths ~ (Age.Index),cex=2,lwd=2,pch=20,type="b",col="red")]
ByAgeChart[,plot(Covid19.Deaths ~ (Age.Index),cex=2,lwd=2,pch=21,type="b",col="blue")]
par(mfrow=c(1,1))


merge(
setnames(cbind(Date=as.Date(now()),as.data.table(t(cbind(
ByAgeChart[1:3,3][,.(Age24andYounger=fsum(.SD))],
ByAgeChart[1:5,3][,.(Age44andYounger=fsum(.SD))],
ByAgeChart[6:10,3][,.(Age45andOlder=fsum(.SD))],
ByAgeChart[8:10,3][,.(Age65andOlder=fsum(.SD))],
ByAgeChart[9:10,3][,.(Age75andOlder=fsum(.SD))],
ByAgeChart[1:10,3][,.(AllAges=fsum(.SD))])),keep.rownames=TRUE)),
c("Date","AgeGroup","COVID19.Deaths"))[],

merge(
setnames(cbind(Date=as.Date(now()),as.data.table(t(cbind(
ByAgeChart[1:3,4][,.(Age24andYounger=fsum(.SD))],
ByAgeChart[1:5,4][,.(Age44andYounger=fsum(.SD))],
ByAgeChart[6:10,4][,.(Age45andOlder=fsum(.SD))],
ByAgeChart[8:10,4][,.(Age65andOlder=fsum(.SD))],
ByAgeChart[9:10,4][,.(Age75andOlder=fsum(.SD))],
ByAgeChart[1:10,4][,.(AllAges=fsum(.SD))])),keep.rownames=TRUE)),
c("Date","AgeGroup","Pneumonia.Deaths"))[],

setnames(cbind(Date=as.Date(now()),as.data.table(t(cbind(
ByAgeChart[1:3,5][,.(Age24andYounger=fsum(.SD))],
ByAgeChart[1:5,5][,.(Age44andYounger=fsum(.SD))],
ByAgeChart[6:10,5][,.(Age45andOlder=fsum(.SD))],
ByAgeChart[8:10,5][,.(Age65andOlder=fsum(.SD))],
ByAgeChart[9:10,5][,.(Age75andOlder=fsum(.SD))],
ByAgeChart[1:10,5][,.(AllAges=fsum(.SD))])),keep.rownames=TRUE)),
c("Date","AgeGroup","All.Deaths"))[],by=c("Date","AgeGroup")),by=c("Date","AgeGroup"))[,
.SD[,.(PctCovidDeaths=round(COVID19.Deaths/All.Deaths,5)* 100,
PctPneumoniaDeaths=round(Pneumonia.Deaths/All.Deaths,5)* 100)],
.(Date,AgeGroup,COVID19.Deaths,Pneumonia.Deaths,All.Deaths)]
#










