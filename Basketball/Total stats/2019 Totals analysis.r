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
Totalstats19 <- read_csv("2019 Totals.csv")

#Tidy the data

#Delete unnecessary columns
Totalstats19$Rk <- NULL

#rename % col
colnames(Totalstats19)[10] <- "FGpct"
colnames(Totalstats19)[13] <- "3Ppct"
colnames(Totalstats19)[16] <- "2Ppct"
colnames(Totalstats19)[17] <- "eFGpct"
colnames(Totalstats19)[20] <- "FTpct"

#na to 0
Totalstats19[is.na(Totalstats19)] <- 0

#Remove after slash
Totalstats19$Player <- sub("\\\\.*", "", Totalstats19$Player)

#Set Pos as factor variable
Totalstats19$Pos <- as.factor(Totalstats19$Pos)
Totalstats19$Tm <- as.factor(Totalstats19$Tm)

#z-scores
Totalstats19 <- mutate(Totalstats19, zAge = (Age - mean(Age))/sd(Age))
Totalstats19 <- mutate(Totalstats19, zGPlayed = (G - mean(G))/sd(G))
Totalstats19 <- mutate(Totalstats19, zMP = (MP - mean(MP))/sd(MP))
Totalstats19 <- mutate(Totalstats19, zFG = (FG - mean(FG))/sd(FG))
Totalstats19 <- mutate(Totalstats19, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats19 <- mutate(Totalstats19, zFGpct = (FGpct - mean(FGpct))/sd(FGpct))
Totalstats19 <- mutate(Totalstats19, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats19 <- mutate(Totalstats19, z3P = (`3P` - mean(`3P`))/sd(`3P`))
Totalstats19 <- mutate(Totalstats19, z3PA = (`3PA` - mean(`3PA`))/sd(`3PA`))
Totalstats19 <- mutate(Totalstats19, z3Ppct = (`3Ppct` - mean(`3Ppct`))/sd(`3Ppct`))
Totalstats19 <- mutate(Totalstats19, z2P = (`2P` - mean(`2P`))/sd(`2P`))
Totalstats19 <- mutate(Totalstats19, z2PA = (`2PA` - mean(`2PA`))/sd(`2PA`))
Totalstats19 <- mutate(Totalstats19, z2Ppct = (`2Ppct` - mean(`2Ppct`))/sd(`2Ppct`))
Totalstats19 <- mutate(Totalstats19, zeFGpct = (eFGpct - mean(eFGpct))/sd(eFGpct))
Totalstats19 <- mutate(Totalstats19, zFT = (FT - mean(FT))/sd(FT))
Totalstats19 <- mutate(Totalstats19, zFTA = (FTA - mean(FTA))/sd(FTA))
Totalstats19 <- mutate(Totalstats19, zFTpct = (FTpct - mean(FTpct))/sd(FTpct))
Totalstats19 <- mutate(Totalstats19, zTRB = (TRB - mean(TRB))/sd(TRB))
Totalstats19 <- mutate(Totalstats19, zAST = (AST - mean(AST))/sd(AST))
Totalstats19 <- mutate(Totalstats19, zSTL = (STL - mean(STL))/sd(STL))
Totalstats19 <- mutate(Totalstats19, zBLK = (BLK - mean(BLK))/sd(BLK))
Totalstats19 <- mutate(Totalstats19, zPTS = (PTS - mean(PTS))/sd(PTS))

#round z-scores
Totalstats19$z2P <- round(Totalstats19$z2P, 2)
Totalstats19$z2PA <- round(Totalstats19$z2PA, 2)
Totalstats19$z2Ppct <- round(Totalstats19$z2Ppct, 2)
Totalstats19$z3P <- round(Totalstats19$z3P, 2)
Totalstats19$z3PA <- round(Totalstats19$z3PA, 2)
Totalstats19$z3Ppct <- round(Totalstats19$z3Ppct, 2)
Totalstats19$z3Ppct <- round(Totalstats19$z3Ppct, 2)
Totalstats19$zAge <- round(Totalstats19$zAge, 2)
Totalstats19$zAST <- round(Totalstats19$zAST, 2)
Totalstats19$zBLK <- round(Totalstats19$zBLK, 2)
Totalstats19$zeFGpct <- round(Totalstats19$zeFGpct, 2)
Totalstats19$zFG <- round(Totalstats19$zFG, 2)
Totalstats19$zFGA <- round(Totalstats19$zFGA, 2)
Totalstats19$zFGpct <- round(Totalstats19$zFGpct, 2)
Totalstats19$zFT <- round(Totalstats19$zFT, 2)
Totalstats19$zFTA <- round(Totalstats19$zFTA, 2)
Totalstats19$zFTpct <- round(Totalstats19$zFTpct, 2)
Totalstats19$zGPlayed <- round(Totalstats19$zGPlayed, 2)
Totalstats19$zMP <- round(Totalstats19$zMP, 2)
Totalstats19$zSTL<- round(Totalstats19$zSTL, 2)
Totalstats19$zTRB<- round(Totalstats19$zTRB, 2)
Totalstats19$zPTS<- round(Totalstats19$zPTS, 2)

#Create a separate tibble for Games > 50, ~70% of games for 2020)
relevantTot19 <- subset(Totalstats19, G > 50)

#Box plot of MP by POS
relevantTot19 %>%
  ggplot(aes(Pos, MP)) +
  geom_boxplot()

#Create Fantasy Value based on Total stats
#Ignore: FG%, FT%, TO
Totalstats19$puntPctFV <- Totalstats19$G*.075 + 
                 Totalstats19$FGA*.1 + Totalstats19$TRB*.125 +
                 Totalstats19$`3PA`*.05 + Totalstats19$AST*.125 +
                 Totalstats19$STL*.125 + Totalstats19$BLK*.125 +
                 Totalstats19$PTS*.125 + Totalstats19$`3P`*.1 +
                 Totalstats19$FTA*.025

relevantTot19$puntPctFV <-  relevantTot19$G*.075 +
                   relevantTot19$FGA*.1 + relevantTot19$TRB*.125 +
                   relevantTot19$`3PA`*.05 + relevantTot19$AST*.125 +
                   relevantTot19$STL*.125 + relevantTot19$BLK*.125 +
                   relevantTot19$PTS*.125 + relevantTot19$`3P`*.125 +
                   relevantTot19$FTA*.025

#Create Fantasy Value based on Total stats
#Ignore: Assists, FT%, TO
Totalstats19$puntASTFV <- Totalstats19$G*.075 + 
                        Totalstats19$FGA*.1 + Totalstats19$TRB*.15 +
                        Totalstats19$`3PA`*.075  +
                        Totalstats19$STL*.15 + Totalstats19$BLK*.15 +
                        Totalstats19$PTS*.125 + Totalstats19$`3P`*.125 +
                        Totalstats19$FTA*.05

relevantTot19$puntASTFV <-  relevantTot19$G*.075 +
  relevantTot19$FGA*.1 + relevantTot19$TRB*.125 +
  relevantTot19$`3PA`*.075 +
  relevantTot19$STL*.125 + relevantTot19$BLK*.125 +
  relevantTot19$PTS*.125 + relevantTot19$`3P`*.125 +
  relevantTot19$FTA*.05

#New Table with Fantasy Value stats
puntPctFV19 <- Totalstats19[c(1:5, 7, 9, 11:12, 19, 23:26, 29, 51)]
zpuntPctFV19 <- Totalstats19[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]
puntPctFVrelev19 <- relevantTot19[c(1:5, 7, 9, 11:12, 19, 23:26, 29, 51)]
zpuntPctFVrelev19 <- relevantTot19[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]

#New Table with Fantasy Value stats
puntASTFV19 <- Totalstats19[c(1:5, 7, 9, 11:12, 19, 23, 25:26, 29, 52)]
zpuntASTFV19 <- Totalstats19[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]
puntASTFVrelev19 <- relevantTot19[c(1:5, 7, 9, 11:12, 19, 23, 25:26, 29, 52)]
zpuntASTFVrelev19 <- relevantTot19[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]

#Summary of FV
summary(puntPctFV19$puntPctFV)
summary(puntPctFVrelev19$puntPctFV)
summary(puntASTFV19$puntASTFV)
summary(puntASTFVrelev19$puntASTFV)

#Box plot of punt Pct FV by POS
relevantTot19 %>%
  ggplot(aes(Pos, puntPctFV)) +
  geom_boxplot()

#Box plot of FV by Pos
puntPctFVrelev19 %>%
  ggplot(aes(Pos, puntPctFV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Box plot of punt AST FV by POS
relevantTot19 %>%
  ggplot(aes(Pos, puntASTFV)) +
  geom_boxplot()

#Box plot of FV by Pos
puntASTFVrelev19 %>%
  ggplot(aes(Pos, puntASTFV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt Pct FV with density curve Total
puntPctFV19 %>%
  ggplot(aes(puntPctFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve (relevant players -> games >50)
puntPctFVrelev19 %>%
  ggplot(aes(puntPctFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve Total
puntASTFV19 %>%
  ggplot(aes(puntASTFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve (relevant players -> games >50)
puntASTFVrelev19 %>%
  ggplot(aes(puntASTFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#corr matrix
corrdata19 <- puntPctFVrelev19[, c(7:15)]
corrmat19 <- round(cor(corrdata19), 2)
head(corrmat19)
meltcorrmat19 <- melt(corrmat19)
head(meltcorrmat19)

#correlation heat map
corrmap19 <- ggplot(data = meltcorrmat19, aes(Var1, Var2, fill = value)) +
  geom_tile()
corrmap19

#get lower triangle of corr matrix
getlowertri <- function(corrmat19){
  corrmat19[upper.tri(corrmat19)] <- NA
  return(corrmat19)
}

#get upper triangle of corr matrix
getuppertri <- function(corrmat19){
  corrmat19[lower.tri(corrmat19)] <- NA
  return(corrmat19)
}

#upper tri
uptri <- getuppertri(corrmat19)
uptri

#lower tri
lowtri <- getlowertri(corrmat19)
lowtri

# Melt corr matrix
meltcorrmat19 <- melt(uptri, na.rm = TRUE)

# Heatmap
ggplot(data = meltcorrmat19, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#scatterplot AST vs PTS w/ trend line
ggplot(data = relevantTot19, aes(AST, PTS))+
  geom_point(size = 2, shape = 16) +
  geom_smooth(method = lm, se = FALSE)

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset")
write_csv(Totalstats19, "TotalStats19.csv")
write_csv(puntPctFV19, "puntPctFV20.csv")
write_csv(puntPctFVrelev19, "puntPctFVrelev19.csv")
write_csv(puntASTFV19, "puntASTFV19.csv")
write_csv(puntASTFVrelev19, "puntASTFVrelev19.csv")
write_csv(zpuntASTFVrelev19, "zpuntASTFVrelev19.csv")
write_csv(zpuntPctFVrelev19, "zpuntPctFVrelev19.csv")

