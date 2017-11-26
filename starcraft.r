setwd("C:/Users/fabrice/Documents/GitHub/sc2StatsLesStatsAStats")
donnee <- read.csv("ExcelJoueurs.csv", header = TRUE, sep = ",")
noms <- donnee$Joueur
ages <- donnee$Age
ratios <- donnee$Ratio.All
ratiosp <- donnee$RatioP
ratiosz <- donnee$RatioZ
ratiost <- donnee$RatioT

races <- donnee$Race



barplot(table(races))

mean(ages)

hist(ratios,col="midnightblue",main="Histogramme ratios", 
     border="red", xlab="ratio",ylab="Effectif")

hist(ages,col="midnightblue",main="Histogramme ages", 
     border="red", xlab="ages",ylab="Effectif")

hist(ratiosp,col="midnightblue",main="Histogramme ratiop", 
     border="red", xlab="ages",ylab="Effectif")

hist(ratiosz,col="midnightblue",main="Histogramme ratioz", 
     border="red", xlab="ages",ylab="Effectif")

hist(ratiost,col="midnightblue",main="Histogramme ratiot", 
     border="red", xlab="ages",ylab="Effectif")

