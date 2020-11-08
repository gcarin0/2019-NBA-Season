#Giancarlo Carino
#Basketball advanced stats
#2020 Season

#load libraries
library(tidyverse)
library(stringr)

#Load data
advbk20stats <- read_csv("advbk20stats.csv")

#Tidy the data

#Delete unnecessary columns
advbk20stats$Rk <- NULL
#advbk18stats$WS <- NULL
#advbk18stats$`WS/48` <- NULL

#Remove after slash
advbk20stats$Player <- sub("\\\\.*", "", advbk20stats$Player)

#Set Pos as factor variable
advbk20stats$Pos <- as.factor(advbk20stats$Pos)

glimpse(advbk20stats)

#Export as CSV after clean up
write_csv(advbk20stats, "Cleaned Dataset/ADVbbstats20.csv")