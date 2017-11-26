setwd("C:/Users/fabrice/Documents/GitHub/sc2StatsLesStatsAStats")
donnee <- read.csv("ExcelJoueurs.csv", header = TRUE, sep = ",")
noms <- donnee$Joueur
ages <- donnee$Age
ages
mean(ages)
