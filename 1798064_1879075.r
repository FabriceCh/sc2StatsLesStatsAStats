##################################################
#Antoine Daigneault-Demers et Fabrice Charbonneau
##################################################


##################################################
#Fonctions

#calcule le mode d'une variable
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#calcule et affiche des statistiques descriptives d'une seule variable
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
##################################################

install.packages("gplots")


##################################################
#working directory
setwd("C:/Users/fabrice/Documents/GitHub/sc2StatsLesStatsAStats") #� modifier...
##################################################


##################################################
#setup des donn�es
donnee <- read.csv("ExcelJoueurs.csv", header = TRUE, sep = ",")
noms <- donnee$Joueur
ages <- donnee$Age
ratios <- donnee$Ratio.All
ratiosp <- donnee$RatioP
ratiosz <- donnee$RatioZ
ratiost <- donnee$RatioT
partiesJouees <- donnee$PartiesAll
races <- donnee$Race
##################################################


##################################################
#Statistiques descriptives
CalculerStats(ages)
CalculerStats(ratios)
CalculerStats(partiesJouees)

#Analyse de chaque race s�par�e:
donneeZerg <- subset(donnee, races == 'Z')
donneeProtoss <- subset(donnee, races == 'P')
donneeTerran <- subset(donnee, races == 'T')

#ages
boxplot(donneeZerg$Age, donneeProtoss$Age, donneeTerran$Age, main="�ges selon la race", names = c("Zerg", "Protoss", "Terran"))

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

####Graphiques###
barplot(table(races))

#diagramme |----[][]------|
boxplot(ratios, main="Ratios")
boxplot(ages, main="Ages")
boxplot(partiesJouees, main="Parties jou�es")

#SECTION AGE / RATIO DE VICTOIRE
Age <- donnee$Age
RatioVsAll <- donnee$Ratio.All

reg.lin <- lm(RatioVsAll~Age, data=donnee)

plot(Age, RatioVsAll, col="blue", pch=16, main="Ratio de victoire selon l'�ge des joueurs", xlab="Age", ylab="Ratio de victoire")
abline(reg.lin, col="red")

#Analyse de la r�gression lin�aire
summary(reg.lin)
confint(reg.lin, level =.90)
anova(reg.lin)

donneeAgeRatio <- donnee[,c("Age","Ratio.All")]


#SECTION NB DE PARTIES JOU�ES / RATIO DE VICTOIRE
partiesJouees <- donnee$PartiesAll
ratioVictoire <- donnee$Ratio.All
regLinParties <- lm(ratioVictoire~partiesJouees, data=donnee)

plot(partiesJouees, ratioVictoire, pch=16, col="blue", main="Ratio de victoire selon le nombre de parties jou�es", xlab="Parties jou�es", ylab="Ratio de victoire")
abline(regLinParties, col="red")

summary(regLinParties)

##################################################



##################################################
#SECTION PARAM�TRES
#tests de shapiro pour v�rifier si les donn�es suivent une loi normale
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

hist(partiesJouees,col="skyblue1",main="Nombre de parties jou�es", 
     border="black", xlab="ages",ylab="Effectif")
mpartiesJouees<-mean(partiesJouees)
stdpartiesJouees<-sqrt(var(partiesJouees))
curve(dnorm(x, mean=mpartiesJouees, sd=stdpartiesJouees), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Analyse avec chi-deux
donneeChiDeuxAge <- read.csv("ageRatioChiDeux.csv", header = TRUE, sep = ";")
chisq.test(donneeChiDeuxAge)

##################################################
















