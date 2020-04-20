####################################
### 1 - Importation des packages ###
####################################

install.packages('pacman')

pacman::p_load('tidyr')
pacman::p_load('dplyr')

#ggplot pour les représentations graphiques
pacman::p_load('ggplot2')
pacman::p_load('plotly')

# MASS pour la fonction stepAIC
pacman::p_load('MASS')

#
pacman::p_load('questionr')

# pour fonction sample.split
pacman::p_load('caTools')

pacman::p_load('lubridate')


############################################
### 2 - Import des données & Préparation ###
############################################

# Renommage des colonnes

df_Loisir <- read.csv("./data/df_Loisir.csv", sep = ';')

df_Loisir <- df_Loisir[,-1]

df_Loisir$date_heure <- ymd_hms(df_Loisir$date_heure)
df_Loisir$date <- ymd(df_Loisir$date)

df_Loisir$Heure <- factor(df_Loisir$Heure)
df_Loisir$num_sem <- factor(df_Loisir$num_sem)
df_Loisir$Mois <- factor(df_Loisir$Mois)
df_Loisir$AnnÃ.e <- factor(df_Loisir$AnnÃ.e)

df_Loisir$vacances_zone_a <- as.logical(df_Loisir$vacances_zone_a)
df_Loisir$vacances_zone_b <- as.logical(df_Loisir$vacances_zone_b)
df_Loisir$vacances_zone_c <- as.logical(df_Loisir$vacances_zone_c)
df_Loisir$Vac_Ete <- as.logical(df_Loisir$Vac_Ete)
df_Loisir$Vac_Hiver <- as.logical(df_Loisir$Vac_Hiver)
df_Loisir$Vac_Ascension <- as.logical(df_Loisir$Vac_Ascension)
df_Loisir$Vac_Printemps <- as.logical(df_Loisir$Vac_Printemps)
df_Loisir$Vac_Toussaint <- as.logical(df_Loisir$Vac_Toussaint)

df_Loisir$Vac_Noel <- as.logical(df_Loisir$Vac_Noel)
df_Loisir$est_jour_ferie <- as.logical(df_Loisir$est_jour_ferie)
df_Loisir$Lundi <- as.logical(df_Loisir$Lundi)
df_Loisir$Mardi <- as.logical(df_Loisir$Mardi)
df_Loisir$Mercredi <- as.logical(df_Loisir$Mercredi)
df_Loisir$Jeudi <- as.logical(df_Loisir$Jeudi)
df_Loisir$Vendredi <- as.logical(df_Loisir$Vendredi)
df_Loisir$Samedi <- as.logical(df_Loisir$Samedi)
df_Loisir$Dimanche <- as.logical(df_Loisir$Dimanche)

#####################################
#  - Régression sans préparation  ###
#####################################

model_sature <- lm(nb_appels_entrants_cor~. , data = df_Loisir)
summary(model_sature)

step(model_sature)

modele_retenu1 <- lm(formula = nb_appels_entrants_cor ~ Heure + date_heure + weekday + 
                      nom_vacances + nom_jour_ferie + num_sem + veille_jour_ferie + 
                      lendemain_jour_ferie + trafic_passager_heure + nb_transaction_carte_abo_heure + 
                      nb_transaction_voyages_heure, data = df_Loisir)
summary(modele_retenu1)#R² 0,1465 R² ajusté = 0.1458

stepAIC(model_sature)

modele_retenu2 <- lm(formula = nb_appels_entrants_cor ~ Heure + demiheure + weekday + 
                       vacances_zone_b + vacances_zone_c + nom_vacances + nom_jour_ferie + 
                       Mois + AnnÃ.e + Trafic_Passager + nb_transaction_carte_abo + 
                       nb_transaction_voyages, data = df_Loisir)
summary(modele_retenu2)


###############
# Interprétation
summary(model_retenu)

#
model_retenu$deviance

#
model_retenu$aic

#calcul de la vraisemblance
prev <- model_retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(df_Loisir))
vrais[df_Loisir$nb_appels_entrants_cor=="Malade"] <- prev[df_Loisir$estMalade10=="Malade"]
vrais[df_Loisir$estMalade10=="Pas malade"] <- 1-prev[df_Loisir$estMalade10=="Pas malade"]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# MAtrice de confusion
appren.p <- cbind(df_heart, predict(model_retenu, newdata = df_heart, type = "link", 
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

tableau <- table(df_heart$Sexe, df_heart$estMalade10)
print(tableau)
tableau <- addmargins(prop.table(addmargins(tableau,1),1),2)
print(tableau)



pacman::p_load(blorr)

blr_step_aic_both(model_retenu)
step(model_retenu)

blr_gains_table(model_retenu, data = df_heart) %>%
  blr_roc_curve()

blr_confusion_matrix(model_retenu, data = df_heart, cutoff = 0)

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



