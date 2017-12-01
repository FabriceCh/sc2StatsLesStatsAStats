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


setwd("C:/Users/Fabri/Onedrive/Documents/GitHub/sc2StatsLesStatsAStats")
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

#SECTION PARAMÈTRES
#tests de shapiro pour vérifier si les données suivent une loi normale
shapiro.test(ages)
shapiro.test(ratios)
shapiro.test(partiesJouees)

#histogrammes
hist(ratios,col="skyblue1",main="Ratios de victoires", 
     border="black", xlab="ratio",ylab="Effectif")
m<-mean(ratios)
std<-sqrt(var(ratios))
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

hist(ages,col="skyblue1",main="Ages", 
     border="black", xlab="ages",ylab="Effectif")
mages<-mean(ages)
stdages<-sqrt(var(ages))
curve(dnorm(x, mean=mages, sd=stdages), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

hist(partiesJouees,col="skyblue1",main="Nombre de parties jouées", 
     border="black", xlab="ages",ylab="Effectif")
mpartiesJouees<-mean(partiesJouees)
stdpartiesJouees<-sqrt(var(partiesJouees))
curve(dnorm(x, mean=mpartiesJouees, sd=stdpartiesJouees), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#test du khi-deux pour voir si des variables dépendent l'une de l'autre
x_tbl <- table(partiesJouees)
x_val <- as.numeric(names(x_tbl))
x_df <- data.frame(count=as.numeric(x_tbl), value=x_val)

# Expand to fill in "gaps" in the values caused by 0 counts
all_x_val <- data.frame(value = 0:max(x_val))
x_df <- merge(all_x_val, x_df, by="value", all.x=TRUE)
x_df$count[is.na(x_df$count)] <- 0

# Get theoretical probabilities 
x_df$eprob <- dgeom(x_df$val, 0.2)

# Chi-square test: once with asymptotic dist'n, 
# once with bootstrap evaluation of chi-sq test statistic
chisq.test(x=x_df$count, p=x_df$eprob, rescale.p=TRUE)



