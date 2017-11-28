setwd("C:/Users/fabrice/Documents/GitHub/sc2StatsLesStatsAStats")
donnee <- read.csv("ExcelJoueurs.csv", header = TRUE, sep = ",")
noms <- donnee$Joueur
ages <- donnee$Age
ratios <- donnee$Ratio.All
ratiosp <- donnee$RatioP
ratiosz <- donnee$RatioZ
ratiost <- donnee$RatioT

races <- donnee$Race


#SECTION AGE / RATIO DE VICTOIRE
ratioAge <- aggregate(donnee[6], list(Age=ages), mean)
Age <- ratioAge$Age
RatioVsAll <- ratioAge$Ratio.All

reg.lin <- lm(RatioVsAll~Age, data=ratioAge)

plot(Age, RatioVsAll, col="blue", main="Ratio de victoire selon l'age et droite de régression", xlab="Age", ylab="Ratio de victoire")
abline(reg.lin, col="red")

#Analyse de la régression linéaire
summary(reg.lin)
confint(reg.lin, level =.90)
anova(reg.lin)


#SECTION NB DE PARTIES JOUÉES / RATIO DE VICTOIRE
partiesJouees <- donnee$PartiesAll
ratioVictoire <- donnee$Ratio.All
regLinParties <- lm(ratioVictoire~partiesJouees, data=donnee)

plot(partiesJouees, ratioVictoire, col="blue", main="Ratio de victoire selon le nombre de parties jouées", xlab="Parties jouées", ylab="Ratio de victoire")
abline(regLinParties, col="red")

summary(regLinParties)


#SECTION ANALYSE AGE / PERFORMANCE AVEC LA RACE
plot(ages, ratios,                                # x variable, y variable
     col = donnee$Race,                          # colour by species
     pch = 16,                                    # type of point to use
     cex = 2,                                     # size of point to use
     xlab = "Age",                                # x axis label
     ylab = "Ratio de victoires",                 # y axis label
     main = "Ratio de victoires selon la race")    # plot title

legend (x = 15, y = 0.7, legend = levels(donnee$Race), col = c(1:3), pch = 16)

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

