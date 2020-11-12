#Giancarlo Carino
#Basketball Total stats
#2018 Season

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
Totalstats$Rk <- NULL

#rename % col
colnames(Totalstats)[10] <- "FGpct"
colnames(Totalstats)[13] <- "3Ppct"
colnames(Totalstats)[16] <- "2Ppct"
colnames(Totalstats)[17] <- "eFGpct"
colnames(Totalstats)[20] <- "FTpct"

#na to 0
Totalstats[is.na(Totalstats)] <- 0

#Remove after slash
Totalstats$Player <- sub("\\\\.*", "", Totalstats$Player)

#Set Pos as factor variable
Totalstats$Pos <- as.factor(Totalstats$Pos)
Totalstats$Tm <- as.factor(Totalstats$Tm)

#z-scores
Totalstats <- mutate(Totalstats, zAge = (Age - mean(Age))/sd(Age))
Totalstats <- mutate(Totalstats, zGPlayed = (G - mean(G))/sd(G))
Totalstats <- mutate(Totalstats, zMP = (MP - mean(MP))/sd(MP))
Totalstats <- mutate(Totalstats, zFG = (FG - mean(FG))/sd(FG))
Totalstats <- mutate(Totalstats, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats <- mutate(Totalstats, zFGpct = (FGpct - mean(FGpct))/sd(FGpct))
Totalstats <- mutate(Totalstats, zFGA = (FGA - mean(FGA))/sd(FGA))
Totalstats <- mutate(Totalstats, z3P = (`3P` - mean(`3P`))/sd(`3P`))
Totalstats <- mutate(Totalstats, z3PA = (`3PA` - mean(`3PA`))/sd(`3PA`))
Totalstats <- mutate(Totalstats, z3Ppct = (`3Ppct` - mean(`3Ppct`))/sd(`3Ppct`))
Totalstats <- mutate(Totalstats, z2P = (`2P` - mean(`2P`))/sd(`2P`))
Totalstats <- mutate(Totalstats, z2PA = (`2PA` - mean(`2PA`))/sd(`2PA`))
Totalstats <- mutate(Totalstats, z2Ppct = (`2Ppct` - mean(`2Ppct`))/sd(`2Ppct`))
Totalstats <- mutate(Totalstats, zeFGpct = (eFGpct - mean(eFGpct))/sd(eFGpct))
Totalstats <- mutate(Totalstats, zFT = (FT - mean(FT))/sd(FT))
Totalstats <- mutate(Totalstats, zFTA = (FTA - mean(FTA))/sd(FTA))
Totalstats <- mutate(Totalstats, zFTpct = (FTpct - mean(FTpct))/sd(FTpct))
Totalstats <- mutate(Totalstats, zTRB = (TRB - mean(TRB))/sd(TRB))
Totalstats <- mutate(Totalstats, zAST = (AST - mean(AST))/sd(AST))
Totalstats <- mutate(Totalstats, zSTL = (STL - mean(STL))/sd(STL))
Totalstats <- mutate(Totalstats, zBLK = (BLK - mean(BLK))/sd(BLK))
Totalstats <- mutate(Totalstats, zPTS = (PTS - mean(PTS))/sd(PTS))

#round z-scores
Totalstats$z2P <- round(Totalstats$z2P, 2)
Totalstats$z2PA <- round(Totalstats$z2PA, 2)
Totalstats$z2Ppct <- round(Totalstats$z2Ppct, 2)
Totalstats$z3P <- round(Totalstats$z3P, 2)
Totalstats$z3PA <- round(Totalstats$z3PA, 2)
Totalstats$z3Ppct <- round(Totalstats$z3Ppct, 2)
Totalstats$z3Ppct <- round(Totalstats$z3Ppct, 2)
Totalstats$zAge <- round(Totalstats$zAge, 2)
Totalstats$zAST <- round(Totalstats$zAST, 2)
Totalstats$zBLK <- round(Totalstats$zBLK, 2)
Totalstats$zeFGpct <- round(Totalstats$zeFGpct, 2)
Totalstats$zFG <- round(Totalstats$zFG, 2)
Totalstats$zFGA <- round(Totalstats$zFGA, 2)
Totalstats$zFGpct <- round(Totalstats$zFGpct, 2)
Totalstats$zFT <- round(Totalstats$zFT, 2)
Totalstats$zFTA <- round(Totalstats$zFTA, 2)
Totalstats$zFTpct <- round(Totalstats$zFTpct, 2)
Totalstats$zGPlayed <- round(Totalstats$zGPlayed, 2)
Totalstats$zMP <- round(Totalstats$zMP, 2)
Totalstats$zSTL<- round(Totalstats$zSTL, 2)
Totalstats$zTRB<- round(Totalstats$zTRB, 2)
Totalstats$zPTS<- round(Totalstats$zPTS, 2)

#Create a separate tibble for Games > 50, ~70% of games for 2020)
relevantTot <- subset(Totalstats, G > 50)

#Box plot of MP by POS
relevantTot %>%
  ggplot(aes(Pos, MP)) +
  geom_boxplot()

#Create Fantasy Value based on Total stats
#Ignore: FG%, FT%, TO
Totalstats$puntPctFV <- Totalstats$G*.075 + 
  Totalstats$FGA*.1 + Totalstats$TRB*.125 +
  Totalstats$`3PA`*.05 + Totalstats$AST*.125 +
  Totalstats$STL*.125 + Totalstats$BLK*.125 +
  Totalstats$PTS*.125 + Totalstats$`3P`*.1 +
  Totalstats$FTA*.025

relevantTot$puntPctFV <-  relevantTot$G*.075 +
  relevantTot$FGA*.1 + relevantTot$TRB*.125 +
  relevantTot$`3PA`*.05 + relevantTot$AST*.125 +
  relevantTot$STL*.125 + relevantTot$BLK*.125 +
  relevantTot$PTS*.125 + relevantTot$`3P`*.125 +
  relevantTot$FTA*.025

#Create Fantasy Value based on Total stats
#Ignore: Assists, FT%, TO
Totalstats$puntASTFV <- Totalstats$G*.075 + 
  Totalstats$FGA*.1 + Totalstats$TRB*.15 +
  Totalstats$`3PA`*.075  +
  Totalstats$STL*.15 + Totalstats$BLK*.15 +
  Totalstats$PTS*.125 + Totalstats$`3P`*.125 +
  Totalstats$FTA*.05

relevantTot$puntASTFV <-  relevantTot$G*.075 +
  relevantTot$FGA*.1 + relevantTot$TRB*.125 +
  relevantTot$`3PA`*.075 +
  relevantTot$STL*.125 + relevantTot$BLK*.125 +
  relevantTot$PTS*.125 + relevantTot$`3P`*.125 +
  relevantTot$FTA*.05

#New Table with Fantasy Value stats
puntPctFV <- Totalstats[c(1:5, 7, 9, 11:12, 18, 23:26, 29, 51)]
zpuntPctFV <- Totalstats[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]
puntPctFVrelev <- relevantTot[c(1:5, 7, 9, 11:12, 18, 23:26, 29, 51)]
zpuntPctFVrelev <- relevantTot[c(1:5, 7, 30:32, 34, 37, 44, 46:51)]

#New Table with Fantasy Value stats
puntASTFV <- Totalstats[c(1:5, 7, 9, 11:12, 18, 23, 25:26, 29, 52)]
zpuntASTFV <- Totalstats[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]
puntASTFVrelev <- relevantTot[c(1:5, 7, 9, 11:12, 18, 23, 25:26, 29, 52)]
zpuntASTFVrelev <- relevantTot[c(1:5, 7, 30:32, 34, 37, 44, 46, 48:50, 52)]

#Summary of FV
summary(puntPctFV$puntPctFV)
summary(puntPctFVrelev$puntPctFV)
summary(puntASTFV$puntASTFV)
summary(puntASTFVrelev$puntASTFV)

#Box plot of punt Pct FV by POS
relevantTot %>%
  ggplot(aes(Pos, puntPctFV)) +
  geom_boxplot()

#Box plot of FV by Pos
puntPctFVrelev %>%
  ggplot(aes(Pos, puntPctFV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Box plot of punt AST FV by POS
relevantTot %>%
  ggplot(aes(Pos, puntASTFV)) +
  geom_boxplot()

#Box plot of FV by Pos
puntASTFVrelev %>%
  ggplot(aes(Pos, puntASTFV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt Pct FV with density curve Total
puntPctFV %>%
  ggplot(aes(puntPctFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve (relevant players -> games >50)
puntPctFVrelev %>%
  ggplot(aes(puntPctFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve Total
puntASTFV %>%
  ggplot(aes(puntASTFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve (relevant players -> games >50)
puntASTFVrelev %>%
  ggplot(aes(puntASTFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#corr matrix
corrdata <- puntPctFVrelev[, c(7:15)]
corrmat <- round(cor(corrdata), 2)
head(corrmat)
meltcorrmat <- melt(corrmat)
head(meltcorrmat)

#correlation heat map
corrmap <- ggplot(data = meltcorrmat, aes(Var1, Var2, fill = value)) +
  geom_tile()
corrmap

#get lower triangle of corr matrix
getlowertri <- function(corrmat){
  corrmat[upper.tri(corrmat)] <- NA
  return(corrmat)
}

#get upper triangle of corr matrix
getuppertri <- function(corrmat){
  corrmat[lower.tri(corrmat)] <- NA
  return(corrmat)
}

#upper tri
uptri <- getuppertri(corrmat)
uptri

#lower tri
lowtri <- getlowertri(corrmat)
lowtri

# Melt corr matrix
meltcorrmat <- melt(uptri, na.rm = TRUE)

# Heatmap
ggplot(data = meltcorrmat, aes(Var2, Var1, fill = value))+
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
ggplot(data = relevantTot, aes(AST, PTS))+
  geom_point(size = 2, shape = 16) +
  geom_smooth(method = lm, se = FALSE)

#Export as CSV after addition of FV
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Cleaned Dataset")
write_csv(Totalstats, "TotalStats18.csv")
write_csv(puntPctFV, "puntPctFV18.csv")
write_csv(puntPctFVrelev, "puntPctFVrelev18.csv")
write_csv(puntASTFV, "puntASTFV18.csv")
write_csv(puntASTFVrelev, "puntASTFVrelev18.csv")
write_csv(zpuntASTFVrelev, "zpuntASTFVrelev18.csv")
write_csv(zpuntPctFVrelev, "zpuntPctFVrelev18.csv")

