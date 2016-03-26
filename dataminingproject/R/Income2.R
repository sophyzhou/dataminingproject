
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
ds <- subset(ds, ds$PINCP >= 1000 & ds$AGEP < 76)


## Marital Status & Employment Status
ds$ESR <- factor(ds$ESR)
ds$MAR <- factor(ds$MAR)
levels(ds$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Employed", "Employed, not at work", "Not in labor force")
levels(ds$MAR) <- c("Married", "Widowed", "Divorced", "Separated", "Never married")

data <- as.data.frame(prop.table(table(ds$MAR, ds$ESR), margin = 1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
  geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) +
  labs(x = "Marital Status", y = "Frequency", title = "Marital Status vs Employment Status")
chisq.test(table(ds$MAR, ds$ESR))

## Degree & Employment Status
ds$SCHL <- ifelse(ds$SCHL <= 16, 16, ds$SCHL)
ds$SCHL <- ifelse(ds$SCHL >= 17 & ds$SCHL <= 19, 19, ds$SCHL)
ds$SCHL <- factor(ds$SCHL)
levels(ds$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")
data <- as.data.frame(prop.table(table(ds$SCHL, ds$ESR), margin = 1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
  geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
  labs(x = "Education", y = "Frequency", title = "Education vs Employment Status") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
chisq.test(table(ds$SCHL, ds$ESR))

## Race & Employment Status
ds$RAC1P <- factor(ds$RAC1P)
levels(ds$RAC1P) <- c("White", "African American", "American Indian", "Alaska Native", "Unspecified Native", "Asian", "Hawaiian Native", "Other", "Multiple")
data <- as.data.frame(prop.table(table(ds$RAC1P, ds$ESR), margin = 1))
ggplot(data, aes(x = Var1, y = Freq, group = Var2)) + 
  geom_bar(stat = "identity", aes(colour = Var2, fill = Var2), alpha = 0.3) + 
  labs(x = "Race", y = "Frequency", title = "Race vs Employment Status") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
chisq.test(table(ds$RAC1P, ds$ESR))

