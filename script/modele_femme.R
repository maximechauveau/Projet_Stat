library(dplyr)
library(tidyr)
library(tidyverse)
library(caTools)
library(ROCR)


###################
# modele de femme #
###################

# on charge le jeu de données
load(file='./data/dataClean.RData')

# on filtre les hommes
df_femme <- df_heart %>% filter(Sexe == 'Femme')
#Suppresion de la colonne Sexe
df_femme <- df_femme[,-1]

# on echantillone
dfmalade <- df_femme %>% filter(estMalade10 == 1)
dfpasmalade <- df_femme %>% filter(estMalade10 == 0)
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]
df <- rbind(dfmalade, dfpasmalade2)

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$estMalade10, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)


# regression logistique
modele.sature <- glm(formula = estMalade10~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = estMalade10 ~ age + Fumeur + diabetes + sysTA, 
                 family = binomial, data = dfApprentissage)
# modele retenu


modele.retenu <- monModele
summary(modele.retenu)

print(modele.retenu$deviance)
print(AIC <- modele.retenu$aic)

#calcul de la vraisemblance
prev <- modele.retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$estMalade10==0] <- prev[dfApprentissage$estMalade10==0]
vrais[dfApprentissage$estMalade10==1] <- 1-prev[dfApprentissage$estMalade10==1]
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
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$estMalade10)))
pourcentage


#Calcul pourcentage
pourcentage <- as.data.frame(pourcentage)
round((pourcentage$Freq[1]+pourcentage$Freq[4])/sum(pourcentage$Freq),4)


# Courbe ROC
# install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$estMalade10)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des rÃ©sidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="Residus studentises par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")
