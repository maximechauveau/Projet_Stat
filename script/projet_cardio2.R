library(dplyr)
library(tidyr)
library(tidyverse)
library(caTools)
library(ROCR)

df <- read.csv('framingham.csv')

# on enleve la variable education
df <- df[,-3]

df <- df %>% drop_na()
summary(df)

# df$male <- ifelse(df$male==0,"femme","homme")
# df$TenYearCHD <- ifelse(df$TenYearCHD=="1","malade","pas malade")

tableau <- table(df$male,df$TenYearCHD)
print(tableau)
tableau <- addmargins(prop.table(addmargins(tableau,1),1),2)
print(tableau)

# on crée classe d'age et sexe

df$male <- apply(df, 1, function(x){if(x[1]==0) 'femme' else 'homme'})
df$male <- as.factor(df$male)
df$age <- apply(df,1, function(x){if(x[2]<=46) 'inf_47ans' 
                                  else if(x[2]>=47 & x[2]<=55) '47-55ans'
                                  else 'sup_55'})
df$age <- as.factor(df$age)

save(df, file='dfcardio.RData')








#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

# modele retenu
glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
      prevalentHyp + totChol + sysBP + glucose, family = binomial, 
    data = dfApprentissage)

modele.retenu <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
                        prevalentHyp + totChol + sysBP + glucose, family = binomial, 
                      data = dfApprentissage)
summary(modele.retenu)

deviance <- modele.retenu$deviance
AIC <- modele.retenu$aic

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                   se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")


#########################
# modele par sexe HOMME # 
#########################

# chargement data
load('dfcardio.RData')

# on filtre par sexe
df <- df %>% filter(male == 'homme')
df <- df[,-1]


#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)


monModele <-  glm(formula = TenYearCHD ~ age + currentSmoker + totChol + sysBP + 
                    heartRate + glucose, family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")


#########################
# modele par sexe FEMME # 
#########################

# chargement data
load('dfcardio.RData')

# on filtre par sexe
df <- df %>% filter(male == 'femme')
df <- df[,-1]


#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <-  glm(formula = TenYearCHD ~ age + cigsPerDay + prevalentHyp + 
                    sysBP + heartRate + glucose, family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")

###################################
# modele par tranche age 47-55ans # 
###################################

# chargement data
load('dfcardio.RData')

# on filtre par tranche d'age
df <- df %>% filter(age == '47-55ans')
df <- df[,-2]


#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = TenYearCHD ~ male + currentSmoker + BPMeds + sysBP + 
                   BMI + glucose, family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")







#################################
# modele par tranche age sup_55 # 
#################################

# chargement data
load('dfcardio.RData')

# on filtre par tranche d'age
df <- df %>% filter(age == 'sup_55')
df <- df[,-2]


#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = TenYearCHD ~ male + cigsPerDay + prevalentStroke + 
                   totChol + sysBP + diaBP + glucose, family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")


#########################################
# modele par tranche age et sexe H->-47 # 
#########################################

# chargement data
load('dfcardio.RData')

# on filtre par tranche d'age
df <- df %>% filter(age == 'inf_47ans' & male == 'homme')
df <- df[,-c(1:2)]


#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = TenYearCHD ~ cigsPerDay + totChol + sysBP + diaBP, 
                 family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")








#########################################
# modele par tranche age et sexe F->-47 # 
#########################################

# chargement data
load('dfcardio.RData')

# on filtre par tranche d'age
df <- df %>% filter(age == 'inf_47ans' & male == 'femme')
df <- df[,-c(1:2)]


#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)

# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = TenYearCHD ~ cigsPerDay + diabetes + sysBP + heartRate, 
                 family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")
pred_test<-predict(modele.retenu,dftest,type = "response")
dftest2<-cbind(dftest,pred_test)

table(dftest$TenYearCHD)
table(dfApprentissage$TenYearCHD)





#########################################
# echantillonnage
#########################################

library(corrplot)
library(Hmisc)

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
symnum(mcor, abbr.colnames=FALSE)


# chargement data
load('dfcardio.RData')

dfmalade <- df %>% filter(TenYearCHD == 1)
dfpasmalade <- df %>% filter(TenYearCHD == 0)
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]
df <- rbind(dfmalade, dfpasmalade2)

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


# regression logistique sans preparation
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + totChol + 
                   sysBP + glucose, family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$TenYearCHD==0] <- prev[dfApprentissage$TenYearCHD==0]
vrais[dfApprentissage$TenYearCHD==1] <- 1-prev[dfApprentissage$TenYearCHD==1]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(modele.retenu, newdata = dfApprentissage, type = "link", 
                                           se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))

#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")
pred_test<-predict(modele.retenu,dftest,type = "response")
dftest2<-cbind(dftest,pred_test)

table(dftest$TenYearCHD)
table(dfApprentissage$TenYearCHD)
