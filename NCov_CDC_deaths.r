# Data from https://www.cdc.gov/nchs/nvss/vsrr/covid19/index.htm
# Covid19: Death vs Age 4/24/2020

library(data.table)
library(lattice)
library(Hmisc)

ByAge <- fread("Provisional_Death_Counts_for_Coronavirus_Disease__COVID-19_ 4.26.2020.CSV",encoding="UTF-8")[Group == "By age",]
names(ByAge) <- ByAge[,gsub("-","_",colnames(.SD))]
names(ByAge) <- ByAge[,gsub(" ","_",colnames(.SD))]
names(ByAge) <- ByAge[,gsub(",","",colnames(.SD))]
names(ByAge) <- ByAge[,gsub("(","",colnames(.SD),fixed =TRUE)]
names(ByAge) <- ByAge[,gsub(")","",colnames(.SD))]
ByAge[,c(5,6,4,7,10,8)][]

ByAge <- setnames(ByAge[,c(4,7,10,8)],c("AgeGroup","Covid19.Deaths","Pneum.Deaths","All.Deaths"))[]
ByAge$Covid19.Deaths <- ByAge[,as.integer(gsub(",","",Covid19.Deaths))]
ByAge$All.Deaths <- ByAge[,as.integer(gsub(",","",All.Deaths))]
ByAge$Pneum.Deaths <- ByAge[,as.integer(gsub(",","",Pneum.Deaths))]
ByAge$AgeGroup <- ByAge[,as.factor(gsub(" ",".",AgeGroup))]
ByAge$AgeGroup <- ByAge[,as.factor(gsub(" - ",".",AgeGroup))]
ByAge[]

ByAgeChart <- ByAge[c(3:12),]
ByAgeChart[,Age.Index := .I]
setcolorder(ByAgeChart,c("Age.Index","AgeGroup","Covid19.Deaths","Pneum.Deaths","All.Deaths"))
ByAgeChart[10,2] <- c("85.years.and.older")

dev.new()
ByAgeChart[,barchart(Covid19.Deaths + Pneum.Deaths ~ as.factor(Age.Index),allow.multiple=TRUE,origin=0)]

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









