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


############################################
### 2 - Import des données & Préparation ###
############################################

# Renommage des colonnes

df_heart <- read.csv("./data/framingham.csv")
# 4238 lignes

df_heart <- df_heart %>% rename(Sexe = male, Fumeur = currentSmoker, NbCigarrete_Jour = cigsPerDay, MedPA = BPMeds, AVC = prevalentStroke,
                                  Hypertension = prevalentHyp, TauxChol = totChol, sysTA = sysBP, diaTA = diaBP, IMC = BMI, BPM = heartRate,
                                  estMalade10 = TenYearCHD)


class(df_heart)
summary(df_heart)

## On enlève les valeurs nulles

df_heart <- df_heart %>% drop_na()
#3656 lignes

# Modification 0 = Homme // 1 = Femme
df_heart$Sexe <- as.character(df_heart$Sexe)

df_heart$Sexe[df_heart$Sexe=='0'] <- 'Homme'
df_heart$Sexe[df_heart$Sexe=='1'] <- 'Femme'
df_heart$Sexe <- as.factor(df_heart$Sexe)

levels(df_heart$Sexe)
freq(df_heart$Sexe)

#Fumeur
df_heart$Fumeur[df_heart$Fumeur=='0'] <- 'Non Fumeur'
df_heart$Fumeur[df_heart$Fumeur=='1'] <- 'Fumeur'
df_heart$Fumeur <- as.factor(df_heart$Fumeur)
df_heart$Fumeur <- relevel(df_heart$Fumeur, 'Non Fumeur')

levels(df_heart$Fumeur)
freq(df_heart$Fumeur)

#MedPA
df_heart$MedPA[df_heart$MedPA == '1'] <- 'Oui'
df_heart$MedPA[df_heart$MedPA == '0'] <- 'Non'
df_heart$MedPA <- as.factor(df_heart$MedPA)
df_heart$MedPA <- relevel(df_heart$MedPA, 'Non')

levels(df_heart$MedPA)
freq(df_heart$MedPA)

#AVC
df_heart$AVC[df_heart$AVC=='1'] <- 'AVC'
df_heart$AVC[df_heart$AVC=='0'] <- 'Pas AVC'
df_heart$AVC <- as.factor(df_heart$AVC)
df_heart$AVC <- relevel(df_heart$AVC, 'Pas AVC')

levels(df_heart$AVC)
freq(df_heart$AVC)

#hypertension
df_heart$Hypertension[df_heart$Hypertension=='0'] <- 'Non'
df_heart$Hypertension[df_heart$Hypertension=='1'] <- 'Oui'
df_heart$Hypertension <- as.factor(df_heart$Hypertension)
df_heart$Hypertension <- relevel(df_heart$Hypertension, 'Non')

levels(df_heart$Hypertension)
freq(df_heart$Hypertension)

#diabete
df_heart$diabetes[df_heart$diabetes=='0'] <- 'Non'
df_heart$diabetes[df_heart$diabetes=='1'] <- 'Oui'
df_heart$diabetes <- as.factor(df_heart$diabetes)
df_heart$diabetes <- relevel(df_heart$diabetes, 'Non')

levels(df_heart$diabetes)
freq(df_heart$diabetes)

# Modification 0 = Pas malade // 1 = Malade

#df_heart$estMalade10[df_heart$estMalade10=='0'] <- 'Pas malade'
#df_heart$estMalade10[df_heart$estMalade10=='1'] <- 'Malade'
df_heart$estMalade10 <- as.factor(df_heart$estMalade10)
df_heart$estMalade10 <- relevel(df_heart$estMalade10, '0')

levels(df_heart$estMalade10)
freq(df_heart$estMalade10)

#education
df_heart$education[df_heart$education=='1'] <- 'Some High School'
df_heart$education[df_heart$education=='2'] <- 'High School or GED'
df_heart$education[df_heart$education=='3'] <- 'Some College or Vocational School'
df_heart$education[df_heart$education=='4'] <- 'college'
df_heart$education <- as.factor(df_heart$education)
df_heart$education <- relevel(df_heart$education, 'Some High School')

levels(df_heart$education)
freq(df_heart$education)

# on echantillone
dfmalade <- df_heart %>% filter(estMalade10 == 1)
dfpasmalade <- df_heart %>% filter(estMalade10 == 0)
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]
df <- rbind(dfmalade, dfpasmalade2)

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$estMalade10, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)

#######################
### 3 - Sauvegarde  ###
#######################

save(df_heart,
     dfApprentissage,
     dftest,
     file = 'data/dataClean.RData')




####################################
### 3 - Statistique descriptive  ###
####################################

# Analyse sexe

hist_sexe <- plot_ly(df_heart, x = df_heart$Sexe, y = table(df_heart$estMalade10), type = 'bar', name = ) %>% 
  add_trace(y = table(df_heart$estMalade10))

hist_sexe

malade <- as.vector(df_heart$estMalade10)

hist_malade <- plot_ly(df_heart, x = names(table(df_heart$estMalade10)), y = table(df_heart$estMalade10), type = 'bar')

hist_malade

pie_education <- plot_ly(df_heart, x = names(table(df_heart$education)), y = table(df_heart$education), type = 'bar')

pie_education



table(df_heart$Sexe)


# Analyse variable non linéaire

hist(df_heart$glucose)

summary(df_heart$glucose)

hist(df_heart$BPM[df_heart$estMalade10=='Malade'])
hist(df_heart$BPM[df_heart$estMalade10=='Pas malade'])
summary(df_heart$BPM)

df_heart$BPM_Cl <- icut(df_heart, BPM)

table(df_heart$BPM_Cl)

# Création d'une nouvelle variable BPM

fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df_heart$BPM[df_heart$estMalade10 == 0], name = 'Pas malade')
fig <- fig %>% add_histogram(x = df_heart$BPM[df_heart$estMalade10 == 1], name = 'Malade')
fig <- fig %>% layout(barmode = "overlay")

fig


# Histo taux chol

# fig <- plot_ly(alpha = 0.6)
# fig <- fig %>% add_histogram(x = df$TauxChol[df$estMalade10=='Pas malade'], name = 'Pas malade')
# fig <- fig %>% add_histogram(x = df$TauxChol[df$estMalade10=='Malade'], name = 'Malade')
# fig <- fig %>% layout(barmode = "overlay")
# 
# fig


x0 <- df$TauxChol[df$estMalade10=='Pas malade']
x1 <- df$TauxChol[df$estMalade10=='Malade']

fit0 <- density(x0)
fit1 <- density(x1)


plot_ly(x = x0, type = "histogram", name = "Histogram", alpha = 0.6) %>% 
  add_trace(x = fit0$x, y = fit0$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Pas malade") %>%
  add_trace(x = fit1$x, y = fit1$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Malade") %>%
  add_histogram(x = df$TauxChol[df$estMalade10=='Malade'], name = 'Malade') %>% 
  layout(yaxis2 = list(overlaying = "y", barmode = "overlay"))

# Valeur seuil 246, taux supérieur à 246, tu as plus de chance d'avoir une crise cardiaque dans les 10 ans que de ne pas en avoir

# Histo age

# fig <- plot_ly(alpha = 0.6)
# fig <- fig %>% add_histogram(x = df$age[df$estMalade10=='Pas malade'], name = 'Pas malade')
# fig <- fig %>% add_histogram(x = df$age[df$estMalade10=='Malade'], name = 'Malade')
# fig <- fig %>% layout(barmode = "overlay")
# 
# fig

x0 <- df$age[df$estMalade10=='Pas malade']
x1 <- df$age[df$estMalade10=='Malade']

fit0 <- density(x0)
fit1 <- density(x1)


plot_ly(x = x0, type = "histogram", name = "Histogram", alpha = 0.6) %>% 
  add_trace(x = fit0$x, y = fit0$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Pas malade") %>%
  add_trace(x = fit1$x, y = fit1$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Malade") %>%
  add_histogram(x = df$age[df$estMalade10=='Malade'], name = 'Malade') %>% 
  layout(yaxis2 = list(overlaying = "y", barmode = "overlay"))
  
# classe age 0-50 - 50 et 100

View(x)



### IMC courbe

x0 <- df$IMC[df$estMalade10=='Pas malade']
x1 <- df$IMC[df$estMalade10=='Malade']

fit0 <- density(x0)
fit1 <- density(x1)


plot_ly(x = x0, type = "histogram", name = "Histogram", alpha = 0.6) %>% 
  add_trace(x = fit0$x, y = fit0$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Pas malade") %>%
  add_trace(x = fit1$x, y = fit1$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Malade") %>%
  add_histogram(x = df$IMC[df$estMalade10=='Malade'], name = 'Malade') %>% 
  layout(yaxis2 = list(overlaying = "y", barmode = "overlay"))

# Valeur seuil : 28 (extreme maigreur : 18)












# Histo nombre cigarrete

fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df$NbCigarrete_Jour[df$estMalade10=='Pas malade'], name = 'Pas malade')
fig <- fig %>% add_histogram(x = df$NbCigarrete_Jour[df$estMalade10=='Malade'], name = 'Malade')
fig <- fig %>% layout(barmode = "overlay")

fig

# Histo diabete

fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df$diabetes[df$estMalade10=='Pas malade'], name = 'Pas malade')
fig <- fig %>% add_histogram(x = df$diabetes[df$estMalade10=='Malade'], name = 'Malade')
fig <- fig %>% layout(barmode = "overlay")

fig

# Histo sysTA

fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = df$sysTA[df$estMalade10=='Pas malade'], name = 'Pas malade')
fig <- fig %>% add_histogram(x = df$sysTA[df$estMalade10=='Malade'], name = 'Malade')
fig <- fig %>% layout(barmode = "overlay")

fig1


### Glucose <- pas ouf
x0 <- df$glucose[df$estMalade10=='Pas malade']
x1 <- df$glucose[df$estMalade10=='Malade']

fit0 <- density(x0)
fit1 <- density(x1)


plot_ly(x = x0, type = "histogram", name = "Histogram", alpha = 0.6) %>% 
  add_trace(x = fit0$x, y = fit0$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Pas malade") %>%
  add_trace(x = fit1$x, y = fit1$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Malade") %>%
  add_histogram(x = df$glucose[df$estMalade10=='Malade'], name = 'Malade') %>% 
  layout(yaxis2 = list(overlaying = "y", barmode = "overlay"))


### Fréquence cardiaque
x0 <- df$BPM[df$estMalade10=='Pas malade']
x1 <- df$BPM[df$estMalade10=='Malade']

fit0 <- density(x0)
fit1 <- density(x1)


plot_ly(x = x0, type = "histogram", name = "Histogram", alpha = 0.6) %>% 
  add_trace(x = fit0$x, y = fit0$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Pas malade") %>%
  add_trace(x = fit1$x, y = fit1$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Malade") %>%
  add_histogram(x = df$BPM[df$estMalade10=='Malade'], name = 'Malade') %>% 
  layout(yaxis2 = list(overlaying = "y", barmode = "overlay"))




# Test d'une variable indicatrice pour la pression artérielle

# df_heart$IndTA <- df_heart$sysTA + df_heart$diaTA
# 
# df_heart <- df_heart[,c(-11, -12)]

#####################################
#  - Régression sans préparation  ###
#####################################

model_sature <- glm(estMalade10~. , data = dfApprentissage, family = binomial)
summary(model_sature)

model_sature <- glm(estMalade10~. , data = df_heart, family = binomial)
summary(model_sature)

step(model_sature)

glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + sysTA, 
    family = binomial, data = dfApprentissage)


stepAIC(model_sature)

glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
    data = df_heart)

model_retenu <- glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + sysTA, 
                    family = binomial, data = dfApprentissage)


###############
# Interprétation
summary(model_retenu)

# Avec base 50/50 : Deviance = 904.8674
model_retenu$deviance

#Avec base 50/50 : AIC = 914
model_retenu$aic

model_retenu$
#calcul de la vraisemblance
prev <- model_retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfApprentissage))
vrais[dfApprentissage$estMalade10== 1] <- prev[dfApprentissage$estMalade10== 1]
vrais[dfApprentissage$estMalade10== 0] <- 1-prev[dfApprentissage$estMalade10== 0]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais)
dev

# Matrice de confusion
appren.p <- cbind(dfApprentissage, predict(model_retenu, newdata = dfApprentissage, type = "link", 
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

tableau <- table(dfApprentissage$Sexe, dfApprentissage$estMalade10)
print(tableau)
tableau <- addmargins(prop.table(addmargins(tableau,1),1),2)
print(tableau)



pacman::p_load(blorr)

blr_step_aic_both(model_retenu)
step(model_retenu)

blr_gains_table(model_retenu, data = dfApprentissage) %>%
  blr_roc_curve()

blr_confusion_matrix(model_retenu, data = dfApprentissage, cutoff = 0)

?blr_confusion_matrix

# Courbe ROC

pacman::p_load(ROCR)
pred <- prediction(appren.p$PredictedWin, appren.p$estMalade10)
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
