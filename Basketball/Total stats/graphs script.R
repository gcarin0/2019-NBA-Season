#Giancarlo Carino
#Basketball Total stats
#graphs script

#set work directory
setwd("C:/Users/Giancarlo/Desktop/R/Projects/Basketball/Total stats")

#Load data from other R file
source("tidy19.R")

#load libraries
library(tidyverse)
library(reshape2)

#Box plot of MP by POS
relevantTot2019 %>%
  ggplot(aes(Pos, MP)) +
  geom_boxplot()

#Box plot of punt Pct FV by POS
relevantTot2019 %>%
  ggplot(aes(Pos, puntPctFV)) +
  geom_boxplot()

#Box plot of FV by Pos
puntPctFVrelev2019 %>%
  ggplot(aes(Pos, puntPctFV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Box plot of punt AST FV by POS
relevantTot2019 %>%
  ggplot(aes(Pos, puntASTFV)) +
  geom_boxplot()

#Box plot of FV by Pos
puntASTFVrelev2019 %>%
  ggplot(aes(Pos, puntASTFV)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt Pct FV with density curve Total
puntPctFV2019 %>%
  ggplot(aes(puntPctFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve (relevant players -> games >50)
puntPctFVrelev2019 %>%
  ggplot(aes(puntPctFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve Total
puntASTFV2019 %>%
  ggplot(aes(puntASTFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Analyze punt AST FV with density curve (relevant players -> games >50)
puntASTFVrelev2019 %>%
  ggplot(aes(puntASTFV)) +
  geom_histogram(binwidth = 10, color = "black", aes(y = ..density..), fill = "gray") +
  geom_density(fill = "light blue", alpha = .1, color = "blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#corr matrix
corrdata2019 <- puntPctFVrelev2019[, c(7:15)]
corrmat2019 <- round(cor(corrdata2019), 2)
head(corrmat2019)
meltcorrmat2019 <- melt(corrmat2019)
head(meltcorrmat2019)

#correlation heat map
corrmap2019 <- ggplot(data = meltcorrmat2019, aes(Var1, Var2, fill = value)) +
  geom_tile()
corrmap2019

#get lower triangle of corr matrix
getlowertri <- function(corrmat2019){
  corrmat2019[upper.tri(corrmat2019)] <- NA
  return(corrmat2019)
}

#get upper triangle of corr matrix
getuppertri <- function(corrmat2019){
  corrmat2019[lower.tri(corrmat2019)] <- NA
  return(corrmat2019)
}

#upper tri
uptri <- getuppertri(corrmat2019)
uptri

#lower tri
lowtri <- getlowertri(corrmat2019)
lowtri

# Melt corr matrix
meltcorrmat2019 <- melt(uptri, na.rm = TRUE)

# Heatmap
ggplot(data = meltcorrmat2019, aes(Var2, Var1, fill = value))+
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
ggplot(data = relevantTot2019, aes(AST, PTS))+
  geom_point(size = 2, shape = 16) +
  geom_smooth(method = lm, se = FALSE)