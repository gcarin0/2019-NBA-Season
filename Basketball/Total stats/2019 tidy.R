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
Totalstats18 <- read_csv("2018 Totals.csv")

#Tidy the data

#Delete unnecessary columns
Totalstats18$Rk <- NULL

#rename % col
colnames(Totalstats18)[10] <- "FGpct"
colnames(Totalstats18)[13] <- "3Ppct"
colnames(Totalstats18)[16] <- "2Ppct"
colnames(Totalstats18)[17] <- "eFGpct"
colnames(Totalstats18)[20] <- "FTpct"

#na to 0
Totalstats18[is.na(Totalstats18)] <- 0

#Remove after slash
Totalstats18$Player <- sub("\\\\.*", "", Totalstats18$Player)

#Set Pos as factor variable
Totalstats18$Pos <- as.factor(Totalstats18$Pos)
Totalstats18$Tm <- as.factor(Totalstats18$Tm)

#z-scores
Totalstats18 <- mutate(Totalstats18, zAge = (Age - mean(Age))/sd(Age))
Totalstats18 <- mutate(Totalstats18, zGPlayed = (G - mean(G))/sd(G))
Totalstats18 <- mutate(Totalstats18, zMP = (MP - mean(MP))/sd(MP))
Totalstats18 <- mutate(Totalstats18, zFG = (FG - mean(FG))/sd(FG))
Totalstats18 <- mutate(Totalstats18, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats18 <- mutate(Totalstats18, zFGpct = (FGpct - mean(FGpct))/sd(FGpct))
Totalstats18 <- mutate(Totalstats18, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats18 <- mutate(Totalstats18, z3P = (`3P` - mean(`3P`))/sd(`3P`))
Totalstats18 <- mutate(Totalstats18, z3PA = (`3PA` - mean(`3PA`))/sd(`3PA`))
Totalstats18 <- mutate(Totalstats18, z3Ppct = (`3Ppct` - mean(`3Ppct`))/sd(`3Ppct`))
Totalstats18 <- mutate(Totalstats18, z2P = (`2P` - mean(`2P`))/sd(`2P`))
Totalstats18 <- mutate(Totalstats18, z2PA = (`2PA` - mean(`2PA`))/sd(`2PA`))
Totalstats18 <- mutate(Totalstats18, z2Ppct = (`2Ppct` - mean(`2Ppct`))/sd(`2Ppct`))
Totalstats18 <- mutate(Totalstats18, zeFGpct = (eFGpct - mean(eFGpct))/sd(eFGpct))
Totalstats18 <- mutate(Totalstats18, zFT = (FT - mean(FT))/sd(FT))
Totalstats18 <- mutate(Totalstats18, zFTA = (FTA - mean(FTA))/sd(FTA))
Totalstats18 <- mutate(Totalstats18, zFTpct = (FTpct - mean(FTpct))/sd(FTpct))
Totalstats18 <- mutate(Totalstats18, zTRB = (TRB - mean(TRB))/sd(TRB))
Totalstats18 <- mutate(Totalstats18, zAST = (AST - mean(AST))/sd(AST))
Totalstats18 <- mutate(Totalstats18, zSTL = (STL - mean(STL))/sd(STL))
Totalstats18 <- mutate(Totalstats18, zBLK = (BLK - mean(BLK))/sd(BLK))
Totalstats18 <- mutate(Totalstats18, zPTS = (PTS - mean(PTS))/sd(PTS))

#round z-scores
Totalstats18$z2P <- round(Totalstats18$z2P, 2)
Totalstats18$z2PA <- round(Totalstats18$z2PA, 2)
Totalstats18$z2Ppct <- round(Totalstats18$z2Ppct, 2)
Totalstats18$z3P <- round(Totalstats18$z3P, 2)
Totalstats18$z3PA <- round(Totalstats18$z3PA, 2)
Totalstats18$z3Ppct <- round(Totalstats18$z3Ppct, 2)
Totalstats18$z3Ppct <- round(Totalstats18$z3Ppct, 2)
Totalstats18$zAge <- round(Totalstats18$zAge, 2)
Totalstats18$zAST <- round(Totalstats18$zAST, 2)
Totalstats18$zBLK <- round(Totalstats18$zBLK, 2)
Totalstats18$zeFGpct <- round(Totalstats18$zeFGpct, 2)
Totalstats18$zFG <- round(Totalstats18$zFG, 2)
Totalstats18$zFGA <- round(Totalstats18$zFGA, 2)
Totalstats18$zFGpct <- round(Totalstats18$zFGpct, 2)
Totalstats18$zFT <- round(Totalstats18$zFT, 2)
Totalstats18$zFTA <- round(Totalstats18$zFTA, 2)
Totalstats18$zFTpct <- round(Totalstats18$zFTpct, 2)
Totalstats18$zGPlayed <- round(Totalstats18$zGPlayed, 2)
Totalstats18$zMP <- round(Totalstats18$zMP, 2)
Totalstats18$zSTL<- round(Totalstats18$zSTL, 2)
Totalstats18$zTRB<- round(Totalstats18$zTRB, 2)
Totalstats18$zPTS<- round(Totalstats18$zPTS, 2)

#Create a separate tibble for Games > 50, ~70% of games for 2020)
relevantTot18 <- subset(Totalstats18, G > 50)

#Create Fantasy Value based on Total stats
#Ignore: FG%, FT%, TO
Totalstats18$puntPctFV <- Totalstats18$G*.075 + 
                 Totalstats18$FGA*.1 + Totalstats18$TRB*.125 +
                 Totalstats18$`3PA`*.05 + Totalstats18$AST*.125 +
                 Totalstats18$STL*.125 + Totalstats18$BLK*.125 +
                 Totalstats18$PTS*.125 + Totalstats18$`3P`*.1 +
                 Totalstats18$FTA*.025

relevantTot18$puntPctFV <-  relevantTot18$G*.075 +
                   relevantTot18$FGA*.1 + relevantTot18$TRB*.125 +
                   relevantTot18$`3PA`*.05 + relevantTot18$AST*.125 +
                   relevantTot18$STL*.125 + relevantTot18$BLK*.125 +
                   relevantTot18$PTS*.125 + relevantTot18$`3P`*.125 +
                   relevantTot18$FTA*.025

#Create Fantasy Value based on Total stats
#Ignore: Assists, FT%, TO
Totalstats18$puntASTFV <- Totalstats18$G*.075 + 
                        Totalstats18$FGA*.1 + Totalstats18$TRB*.15 +
                        Totalstats18$`3PA`*.075  +
                        Totalstats18$STL*.15 + Totalstats18$BLK*.15 +
                        Totalstats18$PTS*.125 + Totalstats18$`3P`*.125 +
                        Totalstats18$FTA*.05

relevantTot18$puntASTFV <-  relevantTot18$G*.075 +
  relevantTot18$FGA*.1 + relevantTot18$TRB*.125 +
  relevantTot18$`3PA`*.075 +
  relevantTot18$STL*.125 + relevantTot18$BLK*.125 +
  relevantTot18$PTS*.125 + relevantTot18$`3P`*.125 +
  relevantTot18$FTA*.05

#New Table with Fantasy Value stats
puntPctFV18 <- Totalstats18[c(1:5, 7, 9, 11:12, 18, 23:26, 29, 51)]
zpuntPctFV18 <- Totalstats18[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]
puntPctFVrelev18 <- relevantTot18[c(1:5, 7, 9, 11:12, 18, 23:26, 29, 51)]
zpuntPctFVrelev18 <- relevantTot18[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]

#New Table with Fantasy Value stats
puntASTFV18 <- Totalstats18[c(1:5, 7, 9, 11:12, 18, 23, 25:26, 29, 52)]
zpuntASTFV18 <- Totalstats18[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]
puntASTFVrelev18 <- relevantTot18[c(1:5, 7, 9, 11:12, 18, 23, 25:26, 29, 52)]
zpuntASTFVrelev18 <- relevantTot18[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]

#Summary of FV
summary(puntPctFV18$puntPctFV)
summary(puntPctFVrelev18$puntPctFV)
summary(puntASTFV18$puntASTFV)
summary(puntASTFVrelev18$puntASTFV)

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset/2018")
write_csv(Totalstats18, "TotalStats18.csv")
write_csv(puntPctFV18, "puntPctFV18.csv")
write_csv(puntPctFVrelev18, "puntPctFVrelev18.csv")
write_csv(puntASTFV18, "puntASTFV18.csv")
write_csv(puntASTFVrelev18, "puntASTFVrelev18.csv")
write_csv(zpuntASTFVrelev18, "zpuntASTFVrelev18.csv")
write_csv(zpuntPctFVrelev18, "zpuntPctFVrelev18.csv")

