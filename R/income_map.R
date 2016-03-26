

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
  colsToKeep <- c("PINCP", "SCHL", "ESR", "ST" ,"SEX", "MAR", "NATIVITY", "RAC1P", "AGEP","CIT","COW")
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
  na.omit()
rm(populData)
ds <- subset(ds, ds$PINCP >= 1000 & ds$AGEP < 76)

##Spatial pattern
stateCodeCSV = "ST,region
001,alabama
002,alaska
004,arizona
005,arkansas
006,california
008,colorado
009,connecticut
010,delaware
011,district of columbia
012,florida
013,georgia
015,hawaii
016,idaho
017,illinois
018,indiana
019,iowa
020,kansas
021,kentucky
022,louisiana
023,maine
024,maryland
025,massachusetts
026,michigan
027,minnesota
028,mississippi
029,missouri
030,montana
031,nebraska
032,nevada
033,new hampshire
034,new jersey
035,new mexico
036,new york
037,north carolina
038,north dakota
039,ohio
040,oklahoma
041,oregon
042,pennsylvania
044,rhode island
045,south carolina
046,south dakota
047,tennessee
048,texas
049,utah
050,vermont
051,virginia
053,washington
054,west virginia
055,wisconsin
056,wyoming"
stateCodes <- fread(stateCodeCSV)



##State Average Incomes Map
income_grouped <- group_by(ds,ST)
data <- summarise(income_grouped,income=mean(PINCP))
data <- right_join(data , stateCodes, by.x=c("ST"))
data[is.na(data)] <- 0
data <- mutate(data, value = data[['income']])
state_choropleth(data, title = "State Average Incomes Map", num_colors=9)

##Umemployment rate map
stateTotal  <- ds%>%
  group_by(ST)%>%
  summarise(count = n())
jobLess  <- ds%>%
  filter(ESR==3)%>%
  group_by(ST)%>%
  summarise(count = n())


jobLess <- right_join(jobLess , stateCodes, by.x=c("ST"))
jobLess[is.na(jobLess)] <- 0
jobLess <- mutate(jobLess, value = jobLess$count/stateTotal$count*100)
state_choropleth(jobLess, title = "State Unemployment Map", num_colors=9)

##Correlation analysis by states
degreeHolder  <- ds%>%
  filter(SCHL>=21)%>%
  group_by(ST)%>%
  summarise(count = n())
cor(data$value,degreeHolder$count/stateTotal$count)
plot(data$value,degreeHolder$count/stateTotal$count)

for(i in 1:9)
{
race  <- ds%>%
  filter(RAC1P==i)%>%
  group_by(ST)%>%
  summarise(count = n())
cor[i]=cor(data$value,race$count/stateTotal$count)
plot(data$value,race$count/stateTotal$count)
}
cor


age  <- ds%>%
  filter(AGEP>=55&AGEP<=75)%>%
  group_by(ST)%>%
  summarise(count = n())
cor(data$value,age$count/stateTotal$count)
plot(data$value,age$count/stateTotal$count)

for(i in 1:5)
{
  marital  <- ds%>%
  filter(MAR==i)%>%
  group_by(ST)%>%
  summarise(count = n())
cor[i]=cor(data$value,marital$count/stateTotal$count)
#plot(data$value,marital$count/stateTotal$count)
}
cor

native  <- ds%>%
  filter(NATIVITY==2)%>%
  group_by(ST)%>%
  summarise(count = n())
cor(data$value,native$count/stateTotal$count)
plot(data$value,native$count/stateTotal$count)


###Rich
rich  <- ds%>%
  filter(PINCP>=120000)%>%
  group_by(ST)%>%
  summarise(count = n())


rich <- right_join(rich , stateCodes, by.x=c("ST"))
rich[is.na(rich)] <- 0
rich <- mutate(rich, value = rich$count/stateTotal$count*100)
state_choropleth(rich, title = "Rich People Percetage Map", num_colors=9)

cor(degreeHolder$count/stateTotal$count,rich$count/stateTotal$count)
plot(degreeHolder$count/stateTotal$count,rich$count/stateTotal$count)
