#Giancarlo Carino
#Basketball Total stats
#2020 Season

#load libraries
library(tidyverse)
library(stringr)

#Load data
Totalstats <- read_csv("2020 Totals.csv")

#Tidy the data

#Delete unnecessary columns
Totalstats$Rk <- NULL

#Remove after slash
Totalstats$Player <- sub("\\\\.*", "", Totalstats$Player)

#Set Pos as factor variable
Totalstats$Pos <- as.factor(Totalstats$Pos)
Totalstats$Tm <- as.factor(Totalstats$Tm)

#Create a separate tibble for Games > 40
relevantTot <- subset(Totalstats, G > 40)

#Box plot of MP by POS
relevantTot %>%
  ggplot(aes(Pos, MP)) +
  geom_boxplot()

#Create Fantasy Value based on Total stats
#Ignore: FG%, FT%, TO
Totalstats$FV <- Totalstats$G*.15 + Totalstats$MP*.15 +
                 Totalstats$FGA*.05 + Totalstats$TRB*.1 +
                 Totalstats$`3PA`*.05 + Totalstats$AST*.1 +
                 Totalstats$STL*.1 + Totalstats$BLK*.1 +
                 Totalstats$PTS*.1 + Totalstats$`3P`*.1 +
                 Totalstats$FTA*.05

relevantTot$FV <-  relevantTot$G*.125 + relevantTot$MP*.125 +
                   relevantTot$FGA*.05 + relevantTot$TRB*.1 +
                   relevantTot$`3PA`*.05 + relevantTot$AST*.1 +
                   relevantTot$STL*.1 + relevantTot$BLK*.1 +
                   relevantTot$PTS*.1 + relevantTot$`3P`*.1 +
                   relevantTot$FTA*.05

#New Table with Fantasy Value stats
FVTotalTable <- Totalstats[c(1:5, 7, 9, 11:12, 15, 19, 23:26, 29:30)]

FVrelevTotalTable <- relevantTot[c(1:5, 7, 9, 11:12, 15, 19, 23:26, 29:30)]

#Summary of FV
summary(relevantTot$FV)
summary(FVTotalTable$FV)
summary(FVrelevTotalTable$FV)

#Box plot of FV by POS
relevantTot %>%
  ggplot(aes(Pos, FV)) +
  geom_boxplot()

#Box plot of FV by Pos
FVrelevTotalTable %>%
  ggplot(aes(Pos, FV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze FV with density curve comparison
FVTotalTable %>%
  ggplot(aes(FV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze FV with density curve (relevant players -> games >50)
FVrelevTotalTable %>%
  ggplot(aes(FV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset")
write_csv(Totalstats, "TotalStats20.csv")
write_csv(FVTotalTable, "FVTotalTable20.csv")
write_csv(FVrelevTotalTable, "FVrelTotalTable20.csv")
