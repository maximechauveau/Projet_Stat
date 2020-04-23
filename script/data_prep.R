####################################
### 1 - Importation des packages ###
####################################

pacman::p_load('pacman')

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

pacman::p_load('data.table')


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

#Création de ma base pour la matrice de corrélation
df_correlation <- df_heart

# Modification 0 = Femme // 1 = Homme
df_heart$Sexe <- as.character(df_heart$Sexe)

df_heart$Sexe[df_heart$Sexe=='0'] <- 'Femme'
df_heart$Sexe[df_heart$Sexe=='1'] <- 'Homme'
df_heart$Sexe <- as.factor(df_heart$Sexe)
df_heart$Sexe <- relevel(df_heart$Sexe, 'Femme')

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

# on echantillone 50 / 50 homme femme
dfmalade <- df_heart %>% filter(estMalade10 == 1)
dfpasmalade <- df_heart %>% filter(estMalade10 == 0)
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]
dfEqui <- as.data.table(rbind(dfmalade, dfpasmalade2))

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(dfEqui$estMalade10, SplitRatio = 0.7)

dfApprenEqui = subset(dfEqui, split == TRUE)
dfTestEqui = subset(dfEqui, split == FALSE)

#Séparation Homme 
set.seed(123)

df_Homme <- df_heart %>% filter(Sexe == 'Homme')

dfmaladeHomme <- df_Homme %>% filter(estMalade10 == 1)
dfpasmaladeHomme <- df_Homme %>% filter(estMalade10 == 0)
dfpasmaladeHomme2 <- dfpasmaladeHomme[sample(1:nrow(dfpasmaladeHomme),nrow(dfmaladeHomme)),]
dfEquiHomme <- as.data.table(rbind(dfmaladeHomme, dfpasmaladeHomme2))

dfEquiHomme <- dfEquiHomme[,-1]

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(dfEquiHomme$estMalade10, SplitRatio = 0.7)

dfApprenEquiHomme = subset(dfEquiHomme, split == TRUE)
dfTestEquiHomme = subset(dfEquiHomme, split == FALSE)

#Séparation Femme 
set.seed(123)

df_Femme <- df_heart %>% filter(Sexe == 'Femme')

dfmaladeFemme <- df_Femme %>% filter(estMalade10 == 1)
dfpasmaladeFemme <- df_Femme %>% filter(estMalade10 == 0)
dfpasmaladeFemme2 <- dfpasmaladeFemme[sample(1:nrow(dfpasmaladeFemme),nrow(dfmaladeFemme)),]
dfEquiFemme <- as.data.table(rbind(dfmaladeFemme, dfpasmaladeFemme2))

dfEquiFemme <- dfEquiFemme[,-1]

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(dfEquiFemme$estMalade10, SplitRatio = 0.7)

dfApprenEquiFemme = subset(dfEquiFemme, split == TRUE)
dfTestEquiFemme = subset(dfEquiFemme, split == FALSE)


#######################
### 3 - Sauvegarde  ###
#######################

save(df_heart,
     df_correlation,
     dfApprenEqui,
     dfTestEqui,
     dfApprenEquiHomme,
     dfTestEquiHomme,
     dfApprenEquiFemme,
     dfTestEquiFemme,
     file = 'data/dataClean.RData')


