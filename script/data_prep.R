####################################
### 1 - Importation des packages ###
####################################
install.packages('pacman')
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


df_heart <- read.csv("./data/framingham.csv")
# 4238 lignes
# Renommage des colonnes
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

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
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


# on verifie que le referenciel est bien pas malade = 0
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


#Classification de l'âge
summary(df_heart$age)

df_heart$agelabel <- cut(df_heart$age, breaks = c(31, 42, 49, 56, 70), labels = c('32-42','43-49','50-56', '57-70'))
df_heart <- df_heart[,-2]
levels(df_heart$agelabel)


#Classification du nombre de cigarette fumee par jour
summary(df_heart$NbCigarrete_Jour)

df_heart$CigaretteLabels <- cut(df_heart$NbCigarrete_Jour, breaks = c(-1, 0, 9, 20, 70), labels = c('0','1-9','10-20', '21-70'))
df_heart <- df_heart[,-4]
levels(df_heart$CigaretteLabels)


#Classification de la tension arterielle systolique
summary(df_heart$sysTA)

df_heart$sysTALabels <- cut(df_heart$sysTA, breaks = c(83, 117, 128, 144, 295), labels = c('83-117','118-128','129-144', '145-295'))
df_heart <- df_heart %>% dplyr::select(-sysTA)
levels(df_heart$sysTALabels)


#Classification de la tension artérielle diastolique
summary(df_heart$diaTA)

df_heart$diaTALabels <- cut(df_heart$diaTA, breaks = c(47, 75, 82, 90, 143), labels = c('48-75','76-82','83-90', '91-144'))
df_heart <- df_heart %>% dplyr::select(-diaTA)
levels(df_heart$diaTALabels)


#Classification du taux de cholesterol
summary(df_heart$TauxChol)

df_heart$TauxCholLabels <- cut(df_heart$TauxChol, breaks = c(112, 206, 234, 263.2, 600), labels = c('113-206','207-234','235-263', '264-600'))
df_heart <- df_heart %>% dplyr::select(-TauxChol)
levels(df_heart$TauxCholLabels)


#Classification de l'IMC
summary(df_heart$IMC)

df_heart$IMCLabels <- cut(df_heart$IMC, breaks = c(15, 23.08, 25.38, 28.04, 56.80), labels = c('15-23','24-25','26-28', '29-57'))
df_heart <- df_heart %>% dplyr::select(-IMC)
levels(df_heart$IMCLabels)


#Classification de la fréquence cardiaque
summary(df_heart$BPM)

df_heart$BPMLabels <- cut(df_heart$BPM, breaks = c(43, 68.00, 75.00, 82.00, 143.00), labels = c('44-68','69-75','76-82', '83-143'))
df_heart <- df_heart %>% dplyr::select(-BPM)
levels(df_heart$BPMLabels)


#Classification du taux de glucose
summary(df_heart$glucose)

df_heart$glucoseLabels <- cut(df_heart$glucose, breaks = c(39, 71, 78, 87, 394), labels = c('40-71', '72-78','79-87', '88-394'))
df_heart <- df_heart %>% dplyr::select(-glucose)
levels(df_heart$glucose)


####################################################
### 3 - Préparation des tables pour les modèles  ###
####################################################


# on echantillone 50 % Malade //  50 % Pas malade

#Création de la table avec les malades
dfmalade <- df_heart %>% filter(estMalade10 == 1)

#Création de la table avec les non malades
dfpasmalade <- df_heart %>% filter(estMalade10 == 0)

#Création d'une table non malades avec le meme nombre de ligne que la table malade
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]

#Fusion des 2 tables = notre table pour notre modele 
dfApprenEqui <- as.data.table(rbind(dfmalade, dfpasmalade2))

#Création de la table test pour notre modele
dfTestEqui <- df_heart


#Séparation Homme 
#Meme procede qu'en dessus mais en filtrant avec les hommes

df_Homme <- df_heart %>% filter(Sexe == 'Homme')

dfmaladeHomme <- df_Homme %>% filter(estMalade10 == 1)
dfpasmaladeHomme <- df_Homme %>% filter(estMalade10 == 0)
dfpasmaladeHomme2 <- dfpasmaladeHomme[sample(1:nrow(dfpasmaladeHomme),nrow(dfmaladeHomme)),]
dfApprenEquiHomme <- as.data.table(rbind(dfmaladeHomme, dfpasmaladeHomme2))

dfApprenEquiHomme <- dfApprenEquiHomme[,-1]

dfTestEquiHomme <- df_Homme


#Séparation Femme 
#Meme procede qu'en dessus mais en filtrant avec les femmes


df_Femme <- df_heart %>% filter(Sexe == 'Femme')

dfmaladeFemme <- df_Femme %>% filter(estMalade10 == 1)
dfpasmaladeFemme <- df_Femme %>% filter(estMalade10 == 0)
dfpasmaladeFemme2 <- dfpasmaladeFemme[sample(1:nrow(dfpasmaladeFemme),nrow(dfmaladeFemme)),]
dfApprenEquiFemme <- as.data.table(rbind(dfmaladeFemme, dfpasmaladeFemme2))

dfApprenEquiFemme <- dfApprenEquiFemme[,-1]

dfTestEquiFemme <- df_Femme


#######################
### 4 - Sauvegarde  ###
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


