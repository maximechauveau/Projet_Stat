##################################################################################
# 1 - Importation des packages
##################################################################################

install.packages('pacman')

pacman::p_load('tidyr')
pacman::p_load('dplyr')

# MASS pour la fonction stepAIC
pacman::p_load('MASS')


##################################################################################
# 2 - Import des données & Préparation
##################################################################################

# Renommage des colonnes

df_cardic <- read.csv("./data/framingham.csv")

df_cardic <- df_cardic %>% rename(Sexe = male, Fumeur = currentSmoker, NbCigarrete_Jour = cigsPerDay, MedPA = BPMeds, AVC = prevalentStroke,
                                  Hypertension = prevalentHyp, TauxChol = totChol, sysTA = sysBP, diaTA = diaBP, IMC = BMI, BPM = heartRate,
                                  estMalade10 = TenYearCHD)


class(df_cardic)
summary(df_cardic)

## On enlève les valeurs nulles

df_cardic <- df_cardic %>% drop_na()


# Test d'une variable indicatrice pour la pression artérielle

# df_cardic$IndTA <- df_cardic$sysTA + df_cardic$diaTA
# 
# df_cardic <- df_cardic[,c(-11, -12)]

#####################################
# 3 - Régression sans préparation ###
#####################################

model_sature <- glm(estMalade10~. , data = df_cardic, family = binomial)
summary(model_sature)

step(model_sature)

glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
    data = df_cardic)


stepAIC(model_sature)

glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
    data = df_cardic)

model_retenu <- glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
                      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
                    data = df_cardic)


###############
# Interprétation
summary(model_retenu)

#
model_retenu$deviance

#
model_retenu$aic

#calcul de la vraisemblance
prev <- model_retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(df_cardic))
vrais[df_cardic$estMalade10=="Malade"] <- prev[df_cardic$estMalade10=="Malade"]
vrais[df_cardic$estMalade10=="Pas malade"] <- 1-prev[df_cardic$estMalade10=="Pas malade"]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# MAtrice de confusion
appren.p <- cbind(df_cardic, predict(model_retenu, newdata = df_cardic, type = "link", 
                                   se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.9, 1, 0)))
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$estMalade10)))

#Calcul pourcentage
# 

tableau <- table(df_cardic$Sexe, df_cardic$estMalade10)
print(tableau)
tableau <- addmargins(prop.table(addmargins(tableau,1),1),2)
print(tableau)



pacman::p_load(blorr)

blr_step_aic_both(model_retenu)
step(model_retenu)

blr_gains_table(model_retenu, data = df_cardic) %>%
  blr_roc_curve()

blr_confusion_matrix(model_retenu, data = df_cardic, cutoff = 0)

?blr_confusion_matrix

# Courbe ROC

pacman::p_load(ROCR)
pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(model_retenu) #residus de deviance
res_pear <- residuals(model_retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(model_retenu) #residu de deviance standardises
H <- influence(model_retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(model_retenu),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(model_retenu),rstudent(model_retenu),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(model_retenu),type="h",ylab="Distance de Cook")
