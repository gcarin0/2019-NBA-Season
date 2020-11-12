#Giancarlo Carino
#Basketball Total stats
#FV year to year diff script
#script for prev year

#Create new data table
diffZpuntASTFV2018 <- zpuntASTFV2018[, c(1:5, 17)]
temp <- zpuntASTFV2018[, c(1,5, 17)]
colnames(temp)[3] <- "puntASTFV2018"
