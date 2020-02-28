##################################################################################
# 1 - Importation des packages
##################################################################################

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


##################################################################################
# 2 - Import des données & Préparation
##################################################################################

# Renommage des colonnes

df_heart <- read.csv("./data/framingham.csv")

df_heart <- df_heart %>% rename(Sexe = male, Fumeur = currentSmoker, NbCigarrete_Jour = cigsPerDay, MedPA = BPMeds, AVC = prevalentStroke,
                                  Hypertension = prevalentHyp, TauxChol = totChol, sysTA = sysBP, diaTA = diaBP, IMC = BMI, BPM = heartRate,
                                  estMalade10 = TenYearCHD)


class(df_heart)
summary(df_heart)

## On enlève les valeurs nulles

df_heart <- df_heart %>% drop_na()

# Modification 0 = Homme // 1 = Femme

df_heart$Sexe <- as.character(df_heart$Sexe)

df_heart$Sexe[df_heart$Sexe=='0'] <- 'Homme'
df_heart$Sexe[df_heart$Sexe=='1'] <- 'Femme'

# Modification 0 = Pas malade // 1 = Malade

df_heart$Sexe <- as.character(df_heart$Sexe)

df_heart$estMalade10[df_heart$estMalade10=='0'] <- 'Pas malade'
df_heart$estMalade10[df_heart$estMalade10=='1'] <- 'Malade'


# 

# on echantillone
dfmalade <- df_heart %>% filter(estMalade10 == 'Malade')
dfpasmalade <- df_heart %>% filter(estMalade10 == 'Pas malade')
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]
df <- rbind(dfmalade, dfpasmalade2)

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$estMalade10, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)




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
fig <- fig %>% add_histogram(x = df$BPM[df$estMalade10=='Pas malade'], name = 'Pas malade')
fig <- fig %>% add_histogram(x = df$BPM[df$estMalade10=='Malade'], name = 'Malade')
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

model_sature <- glm(estMalade10~. , data = df_heart, family = binomial)
summary(model_sature)

step(model_sature)

glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
    data = df_heart)


stepAIC(model_sature)

glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
    data = df_heart)

model_retenu <- glm(formula = estMalade10 ~ Sexe + age + NbCigarrete_Jour + AVC + 
                      Hypertension + TauxChol + sysTA + glucose, family = binomial, 
                    data = df_heart)


###############
# Interprétation
summary(model_retenu)

#
model_retenu$deviance

#
model_retenu$aic

#calcul de la vraisemblance
prev <- model_retenu$fitted.values #on obtient les pi
vrais <- rep(0,nrow(df_heart))
vrais[df_heart$estMalade10=="Malade"] <- prev[df_heart$estMalade10=="Malade"]
vrais[df_heart$estMalade10=="Pas malade"] <- 1-prev[df_heart$estMalade10=="Pas malade"]
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
