install.packages("gplots")

setwd("C:/Users/Fabri/OneDrive/Documents/GitHub/sc2StatsLesStatsAStats")
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
Age <- donnee$Age
RatioVsAll <- donnee$Ratio.All

reg.lin <- lm(RatioVsAll~Age, data=ratioAge)

plot(Age, RatioVsAll, col="blue", pch=16, main="Ratio de victoire selon l'âge des joueurs", xlab="Age", ylab="Ratio de victoire")
abline(reg.lin, col="red")

#Analyse de la régression linéaire
summary(reg.lin)
confint(reg.lin, level =.90)
anova(reg.lin)

donneeAgeRatio <- donnee[,c("Age","Ratio.All")]


#SECTION NB DE PARTIES JOUÉES / RATIO DE VICTOIRE
partiesJouees <- donnee$PartiesAll
ratioVictoire <- donnee$Ratio.All
regLinParties <- lm(ratioVictoire~partiesJouees, data=donnee)

plot(partiesJouees, ratioVictoire, pch=16, col="blue", main="Ratio de victoire selon le nombre de parties jouées", xlab="Parties jouées", ylab="Ratio de victoire")
abline(regLinParties, col="red")

summary(regLinParties)


#SECTION ANALYSE AGE / PERFORMANCE AVEC LA RACE
plot(ages, ratios,                                # x variable, y variable
     col = donnee$Race,                          # colour by species
     pch = 16,                                    # type of point to use
     cex = 2,                                     # size of point to use
     xlab = "Age",                                # x axis label
     ylab = "Ratio de victoires",                 # y axis label
     main = "Ratio de victoires selon l'âge pour chaque race")    # plot title

legend (x = 15, y = 0.7, legend = levels(donnee$Race), col = c(1:3), pch = 16)

donneeZerg <- subset(donnee, races == 'Z')
donneeTerran <- subset(donnee, races == 'T')
donneeProtoss <- subset(donnee, races == 'P')

regLinZerg <- lm(Ratio.All~Age, data=donneeZerg)
regLinTerran <- lm(Ratio.All~Age, data=donneeTerran)
regLinProtoss <- lm(Ratio.All~Age, data=donneeProtoss)

abline(regLinZerg, col="green")
abline(regLinTerran, col="red")
abline(regLinProtoss, col="black")

summary(regLinZerg)
summary(regLinTerran)
summary(regLinProtoss)

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
