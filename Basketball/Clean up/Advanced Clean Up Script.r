#Giancarlo Carino
#Basketball advanced stats
#2019 Season

#load libraries
library(tidyverse)
library(stringr)

#Load data
advbk19stats <- read_csv("advbk19stats.csv")

#Tidy the data

#Delete unnecessary columns
advbk19stats$Rk <- NULL

#Remove after slash
advbk19stats$Player <- sub("\\\\.*", "", advbk20stats$Player)

#Set Pos as factor variable
advbk19stats$Pos <- as.factor(advbk19stats$Pos)

glimpse(advbk19stats)

#Export as CSV after clean up
write_csv(advbk19stats, "Cleaned Dataset/ADVbbstats19.csv")