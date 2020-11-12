#Giancarlo Carino
#Basketball Total stats

#load libraries
library(tidyverse)
library(reshape2)

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Total stats")

#Load data
Totalstats2019 <- read_csv("2019 Totals.csv")

#Tidy the data

#Delete unnecessary columns
Totalstats2019$Rk <- NULL

#rename % col
colnames(Totalstats2019)[5] <- "G2019"
colnames(Totalstats2019)[10] <- "FGpct"
colnames(Totalstats2019)[13] <- "3Ppct"
colnames(Totalstats2019)[16] <- "2Ppct"
colnames(Totalstats2019)[17] <- "eFGpct"
colnames(Totalstats2019)[20] <- "FTpct"

#na to 0
Totalstats2019[is.na(Totalstats2019)] <- 0

#Remove after slash
Totalstats2019$Player <- sub("\\\\.*", "", Totalstats2019$Player)

#Set Pos as factor variable
Totalstats2019$Pos <- as.factor(Totalstats2019$Pos)
Totalstats2019$Tm <- as.factor(Totalstats2019$Tm)

#Players who have been traded have "TOT" as team for total stats from all teams played on.
#"TOT" becomes the first observation for the player and the below code removes each
#duplicated player from each individual team played on
Totalstats2019 <- Totalstats2019 %>%
  distinct(Player, .keep_all = TRUE)

#z-scores
Totalstats2019 <- mutate(Totalstats2019, zAge = (Age - mean(Age))/sd(Age))
Totalstats2019 <- mutate(Totalstats2019, zGPlayed = (G2019 - mean(G2019))/sd(G2019))
Totalstats2019 <- mutate(Totalstats2019, zMP = (MP - mean(MP))/sd(MP))
Totalstats2019 <- mutate(Totalstats2019, zFG = (FG - mean(FG))/sd(FG))
Totalstats2019 <- mutate(Totalstats2019, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats2019 <- mutate(Totalstats2019, zFGpct = (FGpct - mean(FGpct))/sd(FGpct))
Totalstats2019 <- mutate(Totalstats2019, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats2019 <- mutate(Totalstats2019, z3P = (`3P` - mean(`3P`))/sd(`3P`))
Totalstats2019 <- mutate(Totalstats2019, z3PA = (`3PA` - mean(`3PA`))/sd(`3PA`))
Totalstats2019 <- mutate(Totalstats2019, z3Ppct = (`3Ppct` - mean(`3Ppct`))/sd(`3Ppct`))
Totalstats2019 <- mutate(Totalstats2019, z2P = (`2P` - mean(`2P`))/sd(`2P`))
Totalstats2019 <- mutate(Totalstats2019, z2PA = (`2PA` - mean(`2PA`))/sd(`2PA`))
Totalstats2019 <- mutate(Totalstats2019, z2Ppct = (`2Ppct` - mean(`2Ppct`))/sd(`2Ppct`))
Totalstats2019 <- mutate(Totalstats2019, zeFGpct = (eFGpct - mean(eFGpct))/sd(eFGpct))
Totalstats2019 <- mutate(Totalstats2019, zFT = (FT - mean(FT))/sd(FT))
Totalstats2019 <- mutate(Totalstats2019, zFTA = (FTA - mean(FTA))/sd(FTA))
Totalstats2019 <- mutate(Totalstats2019, zFTpct = (FTpct - mean(FTpct))/sd(FTpct))
Totalstats2019 <- mutate(Totalstats2019, zTRB = (TRB - mean(TRB))/sd(TRB))
Totalstats2019 <- mutate(Totalstats2019, zAST = (AST - mean(AST))/sd(AST))
Totalstats2019 <- mutate(Totalstats2019, zSTL = (STL - mean(STL))/sd(STL))
Totalstats2019 <- mutate(Totalstats2019, zBLK = (BLK - mean(BLK))/sd(BLK))
Totalstats2019 <- mutate(Totalstats2019, zPTS = (PTS - mean(PTS))/sd(PTS))

#round z-scores
Totalstats2019$z2P <- round(Totalstats2019$z2P, 2)
Totalstats2019$z2PA <- round(Totalstats2019$z2PA, 2)
Totalstats2019$z2Ppct <- round(Totalstats2019$z2Ppct, 2)
Totalstats2019$z3P <- round(Totalstats2019$z3P, 2)
Totalstats2019$z3PA <- round(Totalstats2019$z3PA, 2)
Totalstats2019$z3Ppct <- round(Totalstats2019$z3Ppct, 2)
Totalstats2019$z3Ppct <- round(Totalstats2019$z3Ppct, 2)
Totalstats2019$zAge <- round(Totalstats2019$zAge, 2)
Totalstats2019$zAST <- round(Totalstats2019$zAST, 2)
Totalstats2019$zBLK <- round(Totalstats2019$zBLK, 2)
Totalstats2019$zeFGpct <- round(Totalstats2019$zeFGpct, 2)
Totalstats2019$zFG <- round(Totalstats2019$zFG, 2)
Totalstats2019$zFGA <- round(Totalstats2019$zFGA, 2)
Totalstats2019$zFGpct <- round(Totalstats2019$zFGpct, 2)
Totalstats2019$zFT <- round(Totalstats2019$zFT, 2)
Totalstats2019$zFTA <- round(Totalstats2019$zFTA, 2)
Totalstats2019$zFTpct <- round(Totalstats2019$zFTpct, 2)
Totalstats2019$zGPlayed <- round(Totalstats2019$zGPlayed, 2)
Totalstats2019$zMP <- round(Totalstats2019$zMP, 2)
Totalstats2019$zSTL<- round(Totalstats2019$zSTL, 2)
Totalstats2019$zTRB<- round(Totalstats2019$zTRB, 2)
Totalstats2019$zPTS<- round(Totalstats2019$zPTS, 2)

#Create a separate tibble for Games > 50, ~70% of games for 2020)
relevantTot2019 <- subset(Totalstats2019, G2019 > 50)

#Create Fantasy Value based on Total stats
#Ignore: FG%, FT%, TO
Totalstats2019$puntPctFV <- Totalstats2019$G2019*.075 + 
                 Totalstats2019$FGA*.1 + Totalstats2019$TRB*.125 +
                 Totalstats2019$`3PA`*.05 + Totalstats2019$AST*.125 +
                 Totalstats2019$STL*.125 + Totalstats2019$BLK*.125 +
                 Totalstats2019$PTS*.125 + Totalstats2019$`3P`*.1 +
                 Totalstats2019$FTA*.025

relevantTot2019$puntPctFV <-  relevantTot2019$G2019*.075 +
                   relevantTot2019$FGA*.1 + relevantTot2019$TRB*.125 +
                   relevantTot2019$`3PA`*.05 + relevantTot2019$AST*.125 +
                   relevantTot2019$STL*.125 + relevantTot2019$BLK*.125 +
                   relevantTot2019$PTS*.125 + relevantTot2019$`3P`*.125 +
                   relevantTot2019$FTA*.025

#Create Fantasy Value based on Total stats
#Ignore: Assists, FT%, TO
Totalstats2019$puntASTFV <- Totalstats2019$G2019*.075 + 
                        Totalstats2019$FGA*.1 + Totalstats2019$TRB*.15 +
                        Totalstats2019$`3PA`*.075  +
                        Totalstats2019$STL*.15 + Totalstats2019$BLK*.15 +
                        Totalstats2019$PTS*.125 + Totalstats2019$`3P`*.125 +
                        Totalstats2019$FTA*.05

relevantTot2019$puntASTFV <-  relevantTot2019$G2019*.075 +
  relevantTot2019$FGA*.1 + relevantTot2019$TRB*.125 +
  relevantTot2019$`3PA`*.075 +
  relevantTot2019$STL*.125 + relevantTot2019$BLK*.125 +
  relevantTot2019$PTS*.125 + relevantTot2019$`3P`*.125 +
  relevantTot2019$FTA*.05

#Round and clean up
Totalstats2019$puntPctFV <- round(Totalstats2019$puntPctFV, 2)
relevantTot2019$puntPctFV  <- round(relevantTot2019$puntPctFV, 2)
Totalstats2019$puntASTFV  <- round(Totalstats2019$puntASTFV, 2)
relevantTot2019$puntASTFV  <- round(relevantTot2019$puntASTFV, 2)

#New Table with Fantasy Value stats
puntPctFV2019 <- Totalstats2019[c(1:5, 7, 9, 11:12, 19, 23:26, 29, 51)]
zpuntPctFV2019 <- Totalstats2019[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]
puntPctFVrelev2019 <- relevantTot2019[c(1:5, 7, 9, 11:12, 19, 23:26, 29, 51)]
zpuntPctFVrelev2019 <- relevantTot2019[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]

#New Table with Fantasy Value stats
puntASTFV2019 <- Totalstats2019[c(1:5, 7, 9, 11:12, 19, 23, 25:26, 29, 52)]
zpuntASTFV2019 <- Totalstats2019[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]
puntASTFVrelev2019 <- relevantTot2019[c(1:5, 7, 9, 11:12, 19, 23, 25:26, 29, 52)]
zpuntASTFVrelev2019 <- relevantTot2019[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]

#Summary of FV
summary(puntPctFV2019$puntPctFV)
summary(puntPctFVrelev2019$puntPctFV)
summary(puntASTFV2019$puntASTFV)
summary(puntASTFVrelev2019$puntASTFV)

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset/2019")
write_csv(Totalstats2019, "TotalStats2019.csv")
write_csv(puntPctFV2019, "puntPctFV2019.csv")
write_csv(puntPctFVrelev2019, "puntPctFVrelev2019.csv")
write_csv(puntASTFV2019, "puntASTFV2019.csv")
write_csv(puntASTFVrelev2019, "puntASTFVrelev2019.csv")
write_csv(zpuntASTFVrelev2019, "zpuntASTFVrelev2019.csv")
write_csv(zpuntPctFVrelev2019, "zpuntPctFVrelev2019.csv")
