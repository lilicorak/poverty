library(tidyverse)
library(reshape2)
library(plyr)

# CIS dataset with official poverty rate, average poverty gap, and relative low income measure
cisData <- subset(read.csv("working_data/CIS_1110013501_databaseLoadingData.csv", head=TRUE, sep=","), 
                  select=c(REF_DATE, GEO, Low.income.lines, Persons.in.low.income, Statistics, VALUE))

cisData$GEO <- revalue(cisData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                       "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                       "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                       "British Columbia" = "BC"))

# seperate age group and sex variables
cisData$Sex <- ifelse(sub(",.*","",cisData$Persons.in.low.income) %in% c("Males", "Females"), 
                      sub(",.*","",cisData$Persons.in.low.income), 
                      "Both sexes")
cisData$Sex <- factor(cisData$Sex, levels=c("Both sexes", 
                                            "Males", 
                                            "Females"))

cisData$Age.group <- ifelse(sub(".*,","",cisData$Persons.in.low.income) %in%
                              c("Persons under 18 years", " under 18 years"),
                            "Under 18 years", 
                            ifelse(sub(".*,","",cisData$Persons.in.low.income) %in% 
                                     c("Persons 18 to 64 years", " 18 to 64 years"),
                                   "18 to 64 years",
                                   ifelse(sub(".*,","",cisData$Persons.in.low.income) %in% 
                                            c("Persons 65 years and over", " 65 years and over"),
                                          "65 years and over",
                                          "All age groups")))

cisData$Age.group <- factor(cisData$Age.group, levels=c("All age groups", 
                                                        "Under 18 years", 
                                                        "18 to 64 years",
                                                        "65 years and over"))

cisData <- subset(cisData, select = -c(Persons.in.low.income))

cisData <- dplyr::rename(cisData, Year = REF_DATE)


# seperate statistics

# official poverty rate
officialrateData <- subset(cisData, 
                           select = -c(Low.income.lines), 
                           Low.income.lines == "Market basket measure, 2008 base" &
                             Statistics == "Percentage of persons in low income")

officialrateData$Statistics <- "Official poverty rate"

# average poverty gap
gapratioData <- subset(cisData, 
                       select = -c(Low.income.lines), 
                       Low.income.lines == "Market basket measure, 2008 base" &
                         Statistics == "Average gap ratio")

gapratioData$Statistics <- "Average poverty gap"

# relative low income
relativelowincData <- subset(cisData, 
                             select = -c(Low.income.lines), 
                             Low.income.lines == "Low income measure after tax" &
                               Statistics == "Percentage of persons in low income")

relativelowincData$Statistics <- "Relative low income"


# LFS data for median hourly wage
lfsData <- subset(read.csv("working_data/LFS_1410034001_databaseLoadingData.csv", head=TRUE, sep=","), 
                  select=c(REF_DATE, GEO, Wages, Type.of.work, Sex, Age.group, VALUE),
                  Type.of.work == "Both full- and part-time employees")

lfsData$GEO <- revalue(lfsData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                       "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                       "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                       "British Columbia" = "BC"))

lfsData$Age.group <- revalue(lfsData$Age.group, c("15 years and over" = "All age groups"))

lfsData$Age.group <- factor(lfsData$Age.group, levels=c("All age groups", 
                                                        "15 to 24 years", 
                                                        "25 to 54 years",
                                                        "55 years and over"))

lfsData <- dplyr::rename(lfsData, Year = REF_DATE)

hourlywageData <- subset(lfsData,
                         select = -c(Wages, Type.of.work))

hourlywageData$Statistics <- "Median hourly wage"


# LAD data for poverty entrance and exit rates
ladData <- subset(read.csv("working_data/LAD_1110002401_databaseLoadingData.csv", head=TRUE, sep=","), 
                  select=c(REF_DATE, GEO, Selected.characteristics, Statistics, VALUE),
                  Selected.characteristics != "Both sexes")

ladData$Sex <- ifelse(ladData$Selected.characteristics %in% c("Males", "Females"), 
                      as.character(ladData$Selected.characteristics), 
                      "Both sexes") 

ladData$Age.group <- ifelse(ladData$Selected.characteristics %in% c("Males", "Females", "Total, 18 years and over"),
                            "All age groups",
                            as.character(ladData$Selected.characteristics))

ladData$GEO <- revalue(ladData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                      "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                      "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                      "British Columbia" = "BC"))

ladData$Age.group <- revalue(ladData$Age.group, c("65 years and above" = "65 years and over"))

ladData$Age.group <- factor(ladData$Age.group, levels=c("All age groups", 
                                                        "18 to 24 years", 
                                                        "25 to 54 years",
                                                        "55 to 64 years",
                                                        "65 years and over"))

ladData$Sex <- factor(ladData$Sex, levels=c("Both sexes", 
                                            "Males", 
                                            "Females"))

ladData$Selected.characteristics <- NULL

ladData <- dplyr::rename(ladData, Year = REF_DATE)


# combine final data sources
finalData <- rbind(officialrateData, gapratioData, relativelowincData, hourlywageData, ladData)
write.csv(finalData, file="data/finalData.csv", na="",row.names = F)



