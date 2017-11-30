#fonction pour calculer le mode (https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

CalculerStats <- function(x) {
  
  print(min(x))
  print(max(x))
  print(mean(x))
  print(median(x))
  print(Mode(x))
  print(max(x)-min(x))
  print(var(x))
  print(sd(x))
  print(sd(x)/mean(x))
}


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

#histogrammes
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

#diagramme |----[][]------|
boxplot(ratios, main="Ratios")
boxplot(ages, main="Ages")
boxplot(partiesJouees, main="Parties jouées")

qqline(partiesJouees, col="red", lwd=2)

qqnorm(partiesJouees, main="nombre de parties jouées")





#Analyse de chaque race séparée:
donneeZerg <- subset(donnee, races == 'Z')
donneeProtoss <- subset(donnee, races == 'P')
donneeTerran <- subset(donnee, races == 'T')

#ages
boxplot(donneeZerg$Age, donneeProtoss$Age, donneeTerran$Age, main="Âges selon la race", names = c("Zerg", "Protoss", "Terran"))

#ratios
boxplot(donneeZerg$Ratio.All, donneeProtoss$Ratio.All, donneeTerran$Ratio.All, main="Ratios selon la race", names = c("Zerg", "Protoss", "Terran"))


mean(donneeZerg$Age)
mean(donneeProtoss$Age)
mean(donneeTerran$Age)

mean(donneeZerg$Ratio.All)
mean(donneeProtoss$Ratio.All)
mean(donneeTerran$Ratio.All)



CalculerStats(donneeZerg$Ratio.All)
CalculerStats(donneeProtoss$Ratio.All)
CalculerStats(donneeTerran$Ratio.All)
CalculerStats(donneeZerg$Age)
CalculerStats(donneeProtoss$Age)
CalculerStats(donneeTerran$Age)


