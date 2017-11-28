setwd("C:/Users/fabrice/Documents/GitHub/sc2StatsLesStatsAStats")
donnee <- read.csv("ExcelJoueurs.csv", header = TRUE, sep = ",")
noms <- donnee$Joueur
ages <- donnee$Age
ratios <- donnee$Ratio.All
ratiosp <- donnee$RatioP
ratiosz <- donnee$RatioZ
ratiost <- donnee$RatioT
partiesJouees <- donnee$PartiesAll

races <- donnee$Race

#STATISTIQUE DESCRIPTIVE

barplot(table(races))

#minimum
min(ages)
min(ratios)
min(partiesJouees)

#maximum
max(ages)
max(ratios)
max(partiesJouees)

#moyennes
mean(ages)
mean(ratios)
mean(partiesJouees)

#mediannes
median(ages)
median(ratios)
median(partiesJouees)

#modes
#fonction pour calculer le mode (https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(ages)
Mode(ratios)
Mode(partiesJouees)

#étendues
max(ages)-min(ages)
max(ratios)-min(ratios)
max(partiesJouees)-min(partiesJouees)

#variances
var(ages)
var(ratios)
var(partiesJouees)

#écarts-type
sd(ages)
sd(ratios)
sd(partiesJouees)

#coefficient de variation
sd(ages)/mean(ages)
sd(ratios)/mean(ratios)
sd(partiesJouees)/mean(partiesJouees)


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

shapiro.test(ages)

ratioAge <- aggregate(ratios, list(Age=ages), mean)
ratioPartie <- aggregate(ratios, list(partiesJouees), mean)

plot(ratioAge)
plot(ratioPartie)

f <- approxfun(ratios, ages)


lines(f, col="red")








#Analyse de chaque race séparée:
donneeZerg <- subset(donnee, races == 'Z')
donneeZerg
donneeProtoss <- subset(donnee, races == 'P')
donneeProtoss
donneeTerran <- subset(donnee, races == 'T')
donneeTerran

mean(donneeZerg$Age)
mean(donneeProtoss$Age)
mean(donneeTerran$Age)


