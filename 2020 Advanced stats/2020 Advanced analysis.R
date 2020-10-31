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

##Find duplicates and store
#Dup <- which(duplicated(advbk20stats$Player))

##Update dataframe without duplicate Players
#UPDadvbk20stats <- advbk20stats[-Dup,]

#Set Pos as factor variable
advbk20stats$Pos <- as.factor(advbk20stats$Pos)

#Export as CSV after clean up
write_csv(advbk20stats, "Cleaned Dataset/ADVbbstats20.csv")

#Create a separate tibble for Games > 50
relevantAdv <- subset(advbk20stats, G > 50)

#Basic statistics MP
summary(relevantAdv$MP)

#Box plot of MP by POS
relevantAdv %>%
  ggplot(aes(Pos, MP)) +
  geom_boxplot()

#Box plot by of PER by POS
relevantAdv %>%
  ggplot(aes(Pos, PER)) +
  geom_boxplot()

#Outliers in PF
outliersPF <-relevantAdv %>%
  filter(Pos == "PF" & PER > 20)

#Outliers in PG
outliersPG <- relevantAdv %>%
  filter(Pos == "PG" & PER > 20)

#Outliers in SG
outliersSG <-relevantAdv %>%
  filter(Pos == "SG" & PER > 20)

#Analyze usage rate with density curve
relevantAdv %>%
  ggplot(aes(`USG%`)) +
  geom_histogram(binwidth = .5, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze PER using histogram with density curve
relevantAdv %>%
  ggplot(aes(`PER`)) +
  geom_histogram(binwidth = .5, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Box plot of Usage rate by Pos
relevantAdv %>%
  ggplot(aes(Pos, `USG%`)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Outliers in PG
relevantAdv %>%
  filter(Pos == "PG" & `USG%` > 32)

#Create Fantasy value of adv stats (Ignore: TO, Strength: PTS, 3s, RBD, STL, BLK)
relevantAdv$fv <- relevantAdv$`3PAr`*.1 + relevantAdv$`USG%`*.1 + 
                  relevantAdv$`TRB%`*.1 + relevantAdv$`STL%`*.1 + 
                  relevantAdv$`BLK%`*.1 + relevantAdv$FTr*.1 + 
                  relevantAdv$`AST%`*.1 + relevantAdv$G*.05 + 
                  relevantAdv$MP*.025 + relevantAdv$PER*.175 + 
                  relevantAdv$`TS%`*.05
advbk20stats$fv <- advbk20stats$`3PAr`*.1 + advbk20stats$`USG%`*.1 + 
                   advbk20stats$`TRB%`*.1 + advbk20stats$`STL%`*.1 + 
                   advbk20stats$`BLK%`*.1 + advbk20stats$FTr*.1 + 
                   advbk20stats$`AST%`*.1 + advbk20stats$G*.05 + 
                   advbk20stats$MP*.025 + advbk20stats$PER*.175 + 
                   advbk20stats$`TS%`*.05

#Box plot of FV by Pos
relevantAdv %>%
  ggplot(aes(Pos, fv)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze FV with density curve (relevant players -> games >50)
relevantAdv %>%
  ggplot(aes(fv)) +
  geom_histogram(binwidth = .5, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze FV with density curve comparison
advbk20stats %>%
  ggplot(aes(fv)) +
  geom_histogram(binwidth = .5, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Summary of FV
summary(advbk20stats$fv)
summary(relevantAdv$fv)

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset")
write_csv(advbk20stats, "ADVbbstats20.csv")
