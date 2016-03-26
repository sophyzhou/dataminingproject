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



wm <- subset(ds, ESR == "Employed" & WAGP > 1000)
