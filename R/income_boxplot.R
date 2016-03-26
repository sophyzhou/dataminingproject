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
  colsToKeep <- c("PINCP", "SCHL", "ESR", "ST" ,"SEX", "MAR", "NATIVITY", "RAC1P", "AGEP", "COW", "CIT")
  popDataA <- fread("DM/2013-american-community-survey/pums/ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("DM/2013-american-community-survey/pums/ss13pusb.csv", select=colsToKeep )
  pus <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(pus, file="populData.RData")
}else{
  load("populData.RData")
} 

##Data Manipulation
pus$MAR <- factor(pus$MAR)
levels(pus$MAR) <- c("Married", "Widowed", "Divorced", "Separated", "Never married")

pus$ESR <- factor(pus$ESR)
levels(pus$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Employed", "Employed, not at work", "Not in labor force")
pus$ESRG <- ifelse(pus$ESR == "Employed", 1, 0)

pus$COW <- factor(pus$COW)
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")

pus$SCHL <- ifelse(pus$SCHL <= 16, 16, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 17 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")

pus$RAC1P <- factor(pus$RAC1P)
levels(pus$RAC1P) <- c("White", "African American", "American Indian", "Alaska Native", "Unspecified Native", "Asian", "Hawaiian Native", "Other", "Multiple")

pus$NATIVITY <- factor(pus$NATIVITY)
levels(pus$NATIVITY) <- c("Native", "Non-Native")

pus$SEX <- factor(pus$SEX)
levels(pus$SEX) <- c("Male", "Female")

pus$CIT <- factor(pus$CIT)
levels(pus$CIT) <- c("Citizen","Citizen", "Citizen","Citizen","Non-Citizen")

am <- pus
wm <- subset(am, ESR == "Employed" & PINCP > 1000 & AGEP < 76)
wm$AGEG <- cut(wm$AGEP, breaks = quantile(wm$AGEP))

###fields of work
data <- as.data.frame(prop.table(table(wm$AGEP, wm$COW)))
data$margin <- prop.table(table(wm$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(wm$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Field of Work") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

### Income vs Age boxplots
ggplot(na.omit(wm), aes(x = AGEG, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = AGEG), alpha = 0.5) + 
  labs(x = "Age Group", y = "Income on Log10 Scale", title = "Income vs Age Groups")


### Income vs fields of work
ggplot(na.omit(wm), aes(x = COW, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = COW), alpha = 0.5) + 
  labs(x = "Field of Work", y = "Income on Log10 Scale", title = "Income vs Field of Work") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

### Income vs Marital status
ggplot(na.omit(wm), aes(x = MAR, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = MAR), alpha = 0.5) + 
  labs(x = "Marital Status", y = "Income on Log10 Scale", title = "Income vs Marital Status") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

### Income vs Education
ggplot(na.omit(wm), aes(x = SCHL, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Income on Log10 Scale", title = "Income vs Education") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

## Income & Race 
ggplot(na.omit(wm), aes(x = RAC1P, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = RAC1P), alpha = 0.5) + 
  labs(x = "Race", y = "Income on Log10 Scale", title = "Income vs Race") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

## Income & Nativity 
ggplot(na.omit(wm), aes(x = NATIVITY, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = NATIVITY), alpha = 0.5) + 
  labs(x = "Nativity", y = "Income on Log10 Scale", title = "Income vs Nativity") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

## Income & Gender 
ggplot(na.omit(wm), aes(x = SEX, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = SEX), alpha = 0.5) + 
  labs(x = "Gender", y = "Income on Log10 Scale", title = "Income vs Gender") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

## Income & Citizenship 
ggplot(na.omit(wm), aes(x = CIT, y = log10(PINCP))) + 
  geom_boxplot(aes(fill = CIT), alpha = 0.5) + 
  labs(x = "Citizenship", y = "Income on Log10 Scale", title = "Income vs Citizenship") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)
