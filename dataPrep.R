# install.packages("tidyverse", "reshape2", "plyr", "cansim")

require(tidyverse)
require(reshape2)
require(plyr)
require(cansim)


# CIS dataset with official poverty rate, average poverty gap, and relative low income measure
cisData <- subset(get_cansim("11-10-0135", refresh = TRUE),
                  select=c(REF_DATE, GEO, `Low income lines`, `Persons in low income`, Statistics, VALUE),
                  `Low income lines` %in% c("Low income measure after tax", "Market basket measure, 2008 base") &
                    Statistics %in% c("Percentage of persons in low income", "Average gap ratio") &
                    `Persons in low income` %in% c("All persons", "Persons under 18 years", "Persons 18 to 64 years",
                                                   "Persons 65 years and over", "Males", "Males, under 18 years",
                                                   "Males, 18 to 64 years", "Males, 65 years and over", "Females",
                                                   "Females, under 18 years", "Females, 18 to 64 years", 
                                                   "Females, 65 years and over") &
                    GEO %in% c("Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
                               "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta",
                               "British Columbia") &
                    REF_DATE >= "2006")

cisData$GEO <- revalue(cisData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                       "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                       "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                       "British Columbia" = "BC"))

# seperate age group and sex variables
cisData$Sex <- ifelse(sub(",.*","",cisData$`Persons in low income`) %in% c("Males", "Females"), 
                      sub(",.*","",cisData$`Persons in low income`), 
                      "Both sexes")
cisData$Sex <- factor(cisData$Sex, levels=c("Both sexes", 
                                            "Males", 
                                            "Females"))

cisData$Age.group <- ifelse(sub(".*,","",cisData$`Persons in low income`) %in%
                              c("Persons under 18 years", " under 18 years"),
                            "Under 18 years", 
                            ifelse(sub(".*,","",cisData$`Persons in low income`) %in% 
                                     c("Persons 18 to 64 years", " 18 to 64 years"),
                                   "18 to 64 years",
                                   ifelse(sub(".*,","",cisData$`Persons in low income`) %in% 
                                            c("Persons 65 years and over", " 65 years and over"),
                                          "65 years and over",
                                          "All age groups")))

cisData$Age.group <- factor(cisData$Age.group, levels=c("All age groups", 
                                                        "Under 18 years", 
                                                        "18 to 64 years",
                                                        "65 years and over"))

cisData <- subset(cisData, select = -c(`Persons in low income`))

cisData <- dplyr::rename(cisData, Year = REF_DATE)


# seperate statistics

# official poverty rate
officialrateData <- subset(cisData, 
                           select = -c(`Low income lines`), 
                           `Low income lines` == "Market basket measure, 2008 base" &
                             Statistics == "Percentage of persons in low income")

officialrateData$Statistics <- "Official poverty rate"

# average poverty gap
gapratioData <- subset(cisData, 
                       select = -c(`Low income lines`), 
                       `Low income lines` == "Market basket measure, 2008 base" &
                         Statistics == "Average gap ratio")

gapratioData$Statistics <- "Average poverty gap"

# relative low income
relativelowincData <- subset(cisData, 
                             select = -c(`Low income lines`), 
                             `Low income lines` == "Low income measure after tax" &
                               Statistics == "Percentage of persons in low income")

relativelowincData$Statistics <- "Relative low income"


# CIS data for bottom 40% income share
cisData_b <- subset(get_cansim("11-10-0193", refresh = TRUE),
                    select=c(REF_DATE, GEO, `Income decile`, VALUE),
                    `Income concept` == "Adjusted after-tax income" &
                      Statistics == "Share of income" &
                      `Income decile` %in% c("Lowest decile", "Second decile", "Third decile", "Fourth decile") &
                      GEO %in% c("Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
                                 "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta",
                                 "British Columbia") &
                      REF_DATE >= "2006")

bottom40Data <- aggregate(cisData_b$VALUE, by = list(Year = cisData_b$REF_DATE, GEO = cisData_b$GEO), FUN=sum)

bottom40Data$Statistics <- "Bottom 40% income share"

bottom40Data$Age.group <- "16 years and over"

bottom40Data$Sex <- "Both sexes"

bottom40Data$GEO <- revalue(bottom40Data$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                                "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                                "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                                "British Columbia" = "BC"))

bottom40Data <- dplyr::rename(bottom40Data, VALUE = x)


# LFS data for median hourly wage
lfsData <- subset(get_cansim("14-10-0340", refresh = TRUE),
                  select=c(REF_DATE, GEO, Sex, `Age group`, VALUE),
                  `Type of work` == "Both full- and part-time employees" &
                    Wages == "Average hourly wage rate" &
                    `National Occupational Classification (NOC)` == "Total employees, all occupations" &
                    REF_DATE >= "2006")

lfsData$GEO <- revalue(lfsData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                       "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                       "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                       "British Columbia" = "BC"))

hourlywageData <- dplyr::rename(lfsData, c(Year = REF_DATE, Age.group = `Age group`))

hourlywageData$Statistics <- "Median hourly wage"


# LAD data for poverty entrance and exit rates
ladData <- subset(get_cansim("11-10-0024", refresh = TRUE),
                  select=c(REF_DATE, GEO, `Selected characteristics`, Statistics, VALUE),
                  `Selected characteristics` %in% c("Males", "Females", "Total, 18 years and over", "18 to 24 years",
                                                    "25 to 54 years", "55 to 64 years", "65 years and above") &
                  GEO %in% c("Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
                            "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta",
                            "British Columbia") & 
                    `Low income threshold` == "Variable low income measure" &
                    Statistics %in% c("Low income entry rate", "Low income exit rate") &
                    substr(REF_DATE, 1, 4) >= "2005")

ladData$Sex <- ifelse(ladData$`Selected characteristics` %in% c("Males", "Females"), 
                      as.character(ladData$`Selected characteristics`), 
                      "Both sexes") 

ladData$Age.group <- ifelse(ladData$`Selected characteristics` %in% c("Males", "Females", "Total, 18 years and over"),
                            "18 years and over",
                            as.character(ladData$`Selected characteristics`))

ladData$GEO <- revalue(ladData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                      "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                      "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                      "British Columbia" = "BC"))

ladData$Age.group <- revalue(ladData$Age.group, c("65 years and above" = "65 years and over"))

ladData <- dplyr::rename(ladData, Year = REF_DATE)

entryexitratesData <- subset(ladData, select = -c(`Selected characteristics`))


# write final data files for app

# write data for official poverty measure page
write.csv(officialrateData, file="data/officialRateData.csv", na="", row.names = F)

# write data for opportunity and inclusion page
write.csv(rbind(relativelowincData, bottom40Data), file="data/opportunityData.csv", na="", row.names = F)

# write data for resilience and security page
write.csv(rbind(gapratioData, hourlywageData, entryexitratesData), file="data/resilienceData.csv", na="", row.names = F)

# write data for dignity page - to come




