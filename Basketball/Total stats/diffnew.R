#Giancarlo Carino
#Basketball Total stats
#FV year to year diff script

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Total stats")
#Load data from tidy file
source("tidy19.R")

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Total stats")
#Load data from other R file
source("tidy18.R")

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Total stats")
#Load data from previous year file
source("diffold.R")

#load libraries
library(tidyverse)
library(reshape2)

#Create new data table
diffZpuntASTFV2019 <- zpuntASTFV2019[, c(1:5, 17)]
zDiffpuntASTFV <- zpuntASTFV2019[, c(1:5, 17)]
colnames(zDiffpuntASTFV)[6] <- "puntASTFV2019"
zDiffpuntASTFV <- zDiffpuntASTFV %>%
                  left_join(temp)
#na to 0
zDiffpuntASTFV[is.na(zDiffpuntASTFV)] <- 0

#Year to year diff in FV, age = latest season
zDiffpuntASTFV <- mutate(zDiffpuntASTFV, `FV%Diff` = ((puntASTFV2019 - puntASTFV2018)/puntASTFV2018))
zDiffpuntASTFV <- mutate(zDiffpuntASTFV, FVDiff = puntASTFV2019 - puntASTFV2018)

#round to clean up
zDiffpuntASTFV$FVDiff <- round(zDiffpuntASTFV$FVDiff, 2)

#inf to 0
zDiffpuntASTFV <- mutate(zDiffpuntASTFV, `FV%Diff` = ifelse(is.infinite(`FV%Diff`), 0, `FV%Diff`))

#Write csv
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset/2019")
write_csv(zDiffpuntASTFV, "2019diff2018.csv")

