install.packages("dplyr")
install.packages('readr')
install.packages('maps')
install.packages('RColorBrewer')
install.packages("data.table")
install.packages("choroplethr")
install.packages("devtools")
install_github("choroplethrMaps", "trulia")

library(readr)
library(RColorBrewer)
library(ggplot2)
library(maps)
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")
library(devtools)
library(choroplethrMaps)


##read data and save it as RData to save time nect time:
##attributes: Income, School Level, Work Status,State 
##and "SEX", "MAR", "NATIVITY", "RAC1P", "AGEP"
reRead <- 1
if(reRead==1){
  colsToKeep <- c("PINCP", "SCHL", "ESR", "ST" ,"SEX", "MAR", "NATIVITY", "RAC1P", "AGEP")
  popDataA <- fread("DM/2013-american-community-survey/pums/ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("DM/2013-american-community-survey/pums/ss13pusb.csv", select=colsToKeep )
  populData <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(populData, file="populData.RData")
}else{
  load("populData.RData")
} 

##Data Manipulation
populData <- tbl_df(populData) 
ds <-  populData %>%  
  na.omit() %>%
  filter(SEX %in%  c(1,2)) %>%
  group_by(SEX) 
rm(populData)

##Gender & Income
ds <- subset(ds, ds$PINCP >= 1000 & ds$AGEP < 76)
hist(ds$PINCP)
dMale <- density(log10(ds[ds$SEX==1,]$PINCP))
dFemale <- density(log10(ds[ds$SEX==2,]$PINCP))
plot(dMale, main='Income by Gender', col.main="darkred", xlab="Income (log10 scale)")
polygon(dMale, col= "#00BFFF55", border="#00BFFF", lwd=2)
polygon(dFemale, col= "#00CD0055", border="#00CD00",lwd=2)
abline(v=seq(3,5.5, by=0.5), col='grey', lty=2)
legend("topright", legend=c("Male", "Female"), fill=c("#00BFFF", "#00CD00"), cex=0.8, ncol=2.5, bg="white")

##Degree & Income
dBS <- density(log10(ds[ds$SCHL==21,]$PINCP))
dMS <- density(log10(ds[ds$SCHL==22,]$PINCP))
dPhD <- density(log10(ds[ds$SCHL==24,]$PINCP))

plot(dMS, main='Income by Degree', col.main="darkred", xlab="Income (log10 scale)")
polygon(dBS, col= "#00BFFF55", border="#00BFFF", lwd=2)
polygon(dMS, col= "#00CD0055", border="#00CD00",lwd=2)
polygon(dPhD, col= "#CAFF7055", border="#CAFF70",lwd=2)
abline(v=seq(3,5.5, by=0.5), col='grey', lty=2)
legend("topright", legend=c("BS", "MS","PhD"), fill=c("#00BFFF", "#00CD00","#CAFF70"), ncol=3.5 ,cex=0.7, bg="white")

##Nativity & Income
dNative <- density(log10(ds[ds$NATIVITY==1,]$PINCP))
dNonNative <- density(log10(ds[ds$NATIVITY==2,]$PINCP))
plot(dNative, main='Income by Nativity', col.main="darkred", xlab="Income (log10 scale)")
polygon(dNative, col= "#00BFFF55", border="#00BFFF", lwd=2)
polygon(dNonNative, col= "#00CD0055", border="#00CD00",lwd=2)
abline(v=seq(3,5.5, by=0.5), col='grey', lty=2)
legend("topright", legend=c("Native", "Non Native"), fill=c("#00BFFF", "#00CD00"), cex=0.8, ncol=2.5, bg="white")
