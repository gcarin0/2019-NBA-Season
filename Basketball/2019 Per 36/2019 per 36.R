#Giancarlo Carino
#Basketball Total stats
#2020 Season

#install packages
install.packages(c("tidyverse", "reshape2"))

#load libraries
library(tidyverse)
library(reshape2)

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/2020 Per 36")

#Load data
per36 <- read_csv("2020 per 36.csv")

#Tidy the data

#Delete unnecessary columns
per36$Rk <- NULL

#Remove after slash
per36$Player <- sub("\\\\.*", "", per36$Player)

#Set Pos as factor variable
per36$Pos <- as.factor(per36s$Pos)
per36$Tm <- as.factor(per36$Tm)

#Create a separate tibble for Games > 50, ~70% of games for 2020)
relevantper36 <- subset(per36, G > 50)

#Box plot of FGA by POS
per36 %>%
  ggplot(aes(Pos, FGA)) +
  geom_boxplot()

#Box plot of FGA by POS
bp <- relevantper36 %>%
      ggplot(aes(Pos, FGA)) +
      geom_boxplot(fill = "yellow")

bp + coord_flip() + geom_jitter(shape = 16, position = position_jitter(0.2))

bp + coord_flip() + geom_jitter(shape = 16) + scale_color_grey() +
          theme(panel.background = element_rect(fill = "gray95"),
          plot.background = element_rect(fill = "gray95"))

#Create Fantasy Value based on per 36 stats
#Ignore: FG%, FT%, TO
per36$puntPctFV <- per36$G*.15 +
  per36$FGA*.1 + per36$TRB*.1 +
  per36$`3PA`*.075 + per36$AST*.1 +
  per36$STL*.1 + per36$BLK*.1 +
  per36$PTS*.1 + per36$`3P`*.1 +
  per36$FTA*.075

relevantper36$puntPctFV <-  relevantper36$G*.15  +
  relevantper36$FGA*.1 + relevantper36$TRB*.1 +
  relevantper36$`3PA`*.075 + relevantper36$AST*.1 +
  relevantper36$STL*.1 + relevantper36$BLK*.1 +
  relevantper36$PTS*.1 + relevantper36$`3P`*.1 +
  relevantper36$FTA*.075

#Create Fantasy Value based on per 36 stats
#Ignore: AST, FT%, TO
per36$puntASTFV <- per36$G*.15 +
  per36$FGA*.1 + per36$TRB*.1 +
  per36$`3PA`*.075 + per36$AST*.1 +
  per36$STL*.1 + per36$BLK*.1 +
  per36$PTS*.1 + per36$`3P`*.1 +
  per36$FTA*.075

relevantper36$puntASTFV <-  relevantper36$G*.15  +
  relevantper36$FGA*.1 + relevantper36$TRB*.1 +
  relevantper36$`3PA`*.075 + relevantper36$AST*.1 +
  relevantper36$STL*.1 + relevantper36$BLK*.1 +
  relevantper36$PTS*.1 + relevantper36$`3P`*.1 +
  relevantper36$FTA*.075