#Giancarlo Carino
#Basketball Total stats

#install packages
install.packages(c("tidyverse", "reshape2"))

#load libraries
library(tidyverse)
library(reshape2)

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Total stats")

#Load data
Totalstats2018 <- read_csv("2018 Totals.csv")

#Tidy the data

#Delete unnecessary columns
Totalstats2018$Rk <- NULL

#rename % col
colnames(Totalstats2018)[5] <- "G2018"
colnames(Totalstats2018)[10] <- "FGpct"
colnames(Totalstats2018)[13] <- "3Ppct"
colnames(Totalstats2018)[16] <- "2Ppct"
colnames(Totalstats2018)[17] <- "eFGpct"
colnames(Totalstats2018)[20] <- "FTpct"

#na to 0
Totalstats2018[is.na(Totalstats2018)] <- 0

#Remove after slash
Totalstats2018$Player <- sub("\\\\.*", "", Totalstats2018$Player)

#Set Pos as factor variable
Totalstats2018$Pos <- as.factor(Totalstats2018$Pos)
Totalstats2018$Tm <- as.factor(Totalstats2018$Tm)

#Players who have been traded have "TOT" as team for total stats from all teams played on.
#"TOT" becomes the first observation for the player and the below code removes each
#duplicated player from each individual team played on
Totalstats2018 <- Totalstats2018 %>%
  distinct(Player, .keep_all = TRUE)

#z-scores
Totalstats2018 <- mutate(Totalstats2018, zAge = (Age - mean(Age))/sd(Age))
Totalstats2018 <- mutate(Totalstats2018, zGPlayed = (G2018 - mean(G2018))/sd(G2018))
Totalstats2018 <- mutate(Totalstats2018, zMP = (MP - mean(MP))/sd(MP))
Totalstats2018 <- mutate(Totalstats2018, zFG = (FG - mean(FG))/sd(FG))
Totalstats2018 <- mutate(Totalstats2018, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats2018 <- mutate(Totalstats2018, zFGpct = (FGpct - mean(FGpct))/sd(FGpct))
Totalstats2018 <- mutate(Totalstats2018, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats2018 <- mutate(Totalstats2018, z3P = (`3P` - mean(`3P`))/sd(`3P`))
Totalstats2018 <- mutate(Totalstats2018, z3PA = (`3PA` - mean(`3PA`))/sd(`3PA`))
Totalstats2018 <- mutate(Totalstats2018, z3Ppct = (`3Ppct` - mean(`3Ppct`))/sd(`3Ppct`))
Totalstats2018 <- mutate(Totalstats2018, z2P = (`2P` - mean(`2P`))/sd(`2P`))
Totalstats2018 <- mutate(Totalstats2018, z2PA = (`2PA` - mean(`2PA`))/sd(`2PA`))
Totalstats2018 <- mutate(Totalstats2018, z2Ppct = (`2Ppct` - mean(`2Ppct`))/sd(`2Ppct`))
Totalstats2018 <- mutate(Totalstats2018, zeFGpct = (eFGpct - mean(eFGpct))/sd(eFGpct))
Totalstats2018 <- mutate(Totalstats2018, zFT = (FT - mean(FT))/sd(FT))
Totalstats2018 <- mutate(Totalstats2018, zFTA = (FTA - mean(FTA))/sd(FTA))
Totalstats2018 <- mutate(Totalstats2018, zFTpct = (FTpct - mean(FTpct))/sd(FTpct))
Totalstats2018 <- mutate(Totalstats2018, zTRB = (TRB - mean(TRB))/sd(TRB))
Totalstats2018 <- mutate(Totalstats2018, zAST = (AST - mean(AST))/sd(AST))
Totalstats2018 <- mutate(Totalstats2018, zSTL = (STL - mean(STL))/sd(STL))
Totalstats2018 <- mutate(Totalstats2018, zBLK = (BLK - mean(BLK))/sd(BLK))
Totalstats2018 <- mutate(Totalstats2018, zPTS = (PTS - mean(PTS))/sd(PTS))

#round z-scores
Totalstats2018$z2P <- round(Totalstats2018$z2P, 2)
Totalstats2018$z2PA <- round(Totalstats2018$z2PA, 2)
Totalstats2018$z2Ppct <- round(Totalstats2018$z2Ppct, 2)
Totalstats2018$z3P <- round(Totalstats2018$z3P, 2)
Totalstats2018$z3PA <- round(Totalstats2018$z3PA, 2)
Totalstats2018$z3Ppct <- round(Totalstats2018$z3Ppct, 2)
Totalstats2018$z3Ppct <- round(Totalstats2018$z3Ppct, 2)
Totalstats2018$zAge <- round(Totalstats2018$zAge, 2)
Totalstats2018$zAST <- round(Totalstats2018$zAST, 2)
Totalstats2018$zBLK <- round(Totalstats2018$zBLK, 2)
Totalstats2018$zeFGpct <- round(Totalstats2018$zeFGpct, 2)
Totalstats2018$zFG <- round(Totalstats2018$zFG, 2)
Totalstats2018$zFGA <- round(Totalstats2018$zFGA, 2)
Totalstats2018$zFGpct <- round(Totalstats2018$zFGpct, 2)
Totalstats2018$zFT <- round(Totalstats2018$zFT, 2)
Totalstats2018$zFTA <- round(Totalstats2018$zFTA, 2)
Totalstats2018$zFTpct <- round(Totalstats2018$zFTpct, 2)
Totalstats2018$zGPlayed <- round(Totalstats2018$zGPlayed, 2)
Totalstats2018$zMP <- round(Totalstats2018$zMP, 2)
Totalstats2018$zSTL<- round(Totalstats2018$zSTL, 2)
Totalstats2018$zTRB<- round(Totalstats2018$zTRB, 2)
Totalstats2018$zPTS<- round(Totalstats2018$zPTS, 2)

#Create a separate tibble for Games > 50, ~70% of games for 2020)
relevantTot2018 <- subset(Totalstats2018, G2018 > 50)

#Create Fantasy Value based on Total stats
#Ignore: FG%, FT%, TO
Totalstats2018$puntPctFV <- Totalstats2018$G2018*.075 + 
                 Totalstats2018$FGA*.1 + Totalstats2018$TRB*.125 +
                 Totalstats2018$`3PA`*.05 + Totalstats2018$AST*.125 +
                 Totalstats2018$STL*.125 + Totalstats2018$BLK*.125 +
                 Totalstats2018$PTS*.125 + Totalstats2018$`3P`*.1 +
                 Totalstats2018$FTA*.025

relevantTot2018$puntPctFV <-  relevantTot2018$G2018*.075 +
                   relevantTot2018$FGA*.1 + relevantTot2018$TRB*.125 +
                   relevantTot2018$`3PA`*.05 + relevantTot2018$AST*.125 +
                   relevantTot2018$STL*.125 + relevantTot2018$BLK*.125 +
                   relevantTot2018$PTS*.125 + relevantTot2018$`3P`*.125 +
                   relevantTot2018$FTA*.025

#Create Fantasy Value based on Total stats
#Ignore: Assists, FT%, TO
Totalstats2018$puntASTFV <- Totalstats2018$G2018*.075 + 
                        Totalstats2018$FGA*.1 + Totalstats2018$TRB*.15 +
                        Totalstats2018$`3PA`*.075  +
                        Totalstats2018$STL*.15 + Totalstats2018$BLK*.15 +
                        Totalstats2018$PTS*.125 + Totalstats2018$`3P`*.125 +
                        Totalstats2018$FTA*.05

relevantTot2018$puntASTFV <-  relevantTot2018$G2018*.075 +
  relevantTot2018$FGA*.1 + relevantTot2018$TRB*.125 +
  relevantTot2018$`3PA`*.075 +
  relevantTot2018$STL*.125 + relevantTot2018$BLK*.125 +
  relevantTot2018$PTS*.125 + relevantTot2018$`3P`*.125 +
  relevantTot2018$FTA*.05

#Round and clean up
Totalstats2018$puntPctFV <- round(Totalstats2018$puntPctFV, 2)
relevantTot2018$puntPctFV  <- round(relevantTot2018$puntPctFV, 2)
Totalstats2018$puntASTFV  <- round(Totalstats2018$puntASTFV, 2)
relevantTot2018$puntASTFV  <- round(relevantTot2018$puntASTFV, 2)

#New Table with Fantasy Value stats
puntPctFV2018 <- Totalstats2018[c(1:5, 7, 9, 11:12, 19, 23:26, 29, 51)]
zpuntPctFV2018 <- Totalstats2018[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]
puntPctFVrelev2018 <- relevantTot2018[c(1:5, 7, 9, 11:12, 19, 23:26, 29, 51)]
zpuntPctFVrelev2018 <- relevantTot2018[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]

#New Table with Fantasy Value stats
puntASTFV2018 <- Totalstats2018[c(1:5, 7, 9, 11:12, 19, 23, 25:26, 29, 52)]
zpuntASTFV2018 <- Totalstats2018[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]
puntASTFVrelev2018 <- relevantTot2018[c(1:5, 7, 9, 11:12, 19, 23, 25:26, 29, 52)]
zpuntASTFVrelev2018 <- relevantTot2018[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]

#Summary of FV
summary(puntPctFV2018$puntPctFV)
summary(puntPctFVrelev2018$puntPctFV)
summary(puntASTFV2018$puntASTFV)
summary(puntASTFVrelev2018$puntASTFV)

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset/2018")
write_csv(Totalstats2018, "TotalStats2018.csv")
write_csv(puntPctFV2018, "puntPctFV2018.csv")
write_csv(puntPctFVrelev2018, "puntPctFVrelev2018.csv")
write_csv(puntASTFV2018, "puntASTFV2018.csv")
write_csv(puntASTFVrelev2018, "puntASTFVrelev2018.csv")
write_csv(zpuntASTFVrelev2018, "zpuntASTFVrelev2018.csv")
write_csv(zpuntPctFVrelev2018, "zpuntPctFVrelev2018.csv")
