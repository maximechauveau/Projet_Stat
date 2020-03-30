library(dplyr)
library(tidyr)
library(tidyverse)
library(caTools)
library(ROCR)
library(corrplot)
library(Hmisc)
library(plotly)


#######################
# modele de reférence #
#######################


# chargement data
df <- read.csv('framingham.csv')

# on enleve les lignes avec du NAs
df <- df %>% drop_na()

mcor <- cor(df)
mcor

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# 
hist(df$heartRate[df$TenYearCHD==0])
hist(df$heartRate[df$TenYearCHD==1])

# on crée classe d'age et sexe
df$male <- apply(df, 1, function(x){if(x[1]==0) 'femme' else 'homme'})
df$male <- as.factor(df$male)
df$TenYearCHD <- apply(df, 1, function(x){if(x[16]==0) 'pas malade' else 'malade'})
df$TenYearCHD <- as.factor(df$TenYearCHD)
df$age <- apply(df,1, function(x){if(x[2]<=46) 'inf_47ans' 
  else if(x[2]>=47 & x[2]<=55) '47-55ans'
  else 'sup_55'})
df$age <- as.factor(df$age)

# enregistrement du jeu de données
save(df, file='dfcardio.RData')

p <- plot_ly(df, x = names(table(df$male)), y = table(df$male), type = 'bar')
p

p <- plot_ly(df, labels = names(table(df$male)), values = table(df$male), type = 'pie')
p

p <- plot_ly(df, labels = names(table(df$TenYearCHD)), values = table(df$TenYearCHD), type = 'pie')
p

# graphe tranche d'age malade/pas malade
trancheAge <- unique(df$age)
trancheAge1 <- table(df$TenYearCHD[df$age=='inf_47ans'])
trancheAge2 <- table(df$TenYearCHD[df$age=='47-55ans'])
trancheAge3 <- table(df$TenYearCHD[df$age=='sup_55'])

malade <- c(trancheAge1[1], trancheAge2[1],trancheAge3[1])
pasMalade <- c(trancheAge1[2], trancheAge2[2],trancheAge3[2])
data <- data.frame(trancheAge, malade, pasMalade)

p <- plot_ly(data, x = ~trancheAge, y = ~malade, type = 'bar', name = 'malade') %>% 
  add_trace(y = ~pasMalade, name = 'pas malade') %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p

# graphe sexe malade/pas malade
sexe <- unique(df$male)
homme <- table(df$TenYearCHD[df$male=='homme'])
femme <- table(df$TenYearCHD[df$male=='femme'])

malade <- c(homme[1], femme[1])
pasMalade <- c(homme[2], femme[2])
data <- data.frame(sexe, malade, pasMalade)

p <- plot_ly(data, x = ~sexe, y = ~malade, type = 'bar', name = 'malade') %>% 
  add_trace(y = ~pasMalade, name = 'pas malade') %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p

table(df$currentSmoker,df$TenYearCHD)
table(df$cigsPerDay,df$TenYearCHD)

tableau <- table(df$cigsPerDay,df$TenYearCHD)
tableau <- as.matrix(tableau)

# enregistrement du jeu de données
save(df, file='dfcardio.RData')

# variable glucose
load('dfcardio.RData')
hist(df$glucose)
summary(df$glucose)[2]
# df$glucose2 <- apply(df,1, function(x){if(x[15]<71) 'tranche 1'
#   else if(x[15]>=71 & x[15]<=87) 'tranche 2'
#   else 'tranche 3'})
table(df$glucose2)
df$glucose2[df$glucose<summary(df$glucose)[2]] <- 'tranche 1'
df$glucose2[df$glucose>=summary(df$glucose)[2] & df$glucose<=summary(df$glucose)[5]] <- 'tranche 2'
df$glucose2[df$glucose>summary(df$glucose)[5]] <- 'tranche 3'
table(df$glucose2)
df <- df[,-15]

# totChol
hist(df$totChol)
summary(df$totChol)
df$totChol2[df$totChol<summary(df$totChol)[2]] <- 'tranche 1'
df$totChol2[df$totChol>=summary(df$totChol)[2] & df$totChol<=summary(df$totChol)[5]] <- 'tranche 2'
df$totChol2[df$totChol>summary(df$totChol)[5]] <- 'tranche 3'

# BMI
hist(df$BMI)
summary(df$BMI)
df$BMI2[df$BMI<summary(df$BMI)[2]] <- 'tranche 1'
df$BMI2[df$BMI>=summary(df$BMI)[2] & df$BMI<=summary(df$BMI)[5]] <- 'tranche 2'
df$BMI2[df$BMI>summary(df$BMI)[5]] <- 'tranche 3'

# heartRate
hist(df$heartRate)
summary(df$heartRate)
df$heartRate2[df$heartRate<summary(df$heartRate)[2]] <- 'tranche 1'
df$heartRate2[df$heartRate>=summary(df$heartRate)[2] & df$heartRate<=summary(df$heartRate)[5]] <- 'tranche 2'
df$heartRate2[df$heartRate>summary(df$heartRate)[5]] <- 'tranche 3'

# 
hist(df$cigsPerDay[df$currentSmoker==1])
summary(df$cigsPerDay[df$currentSmoker==1])
df$cigsPerDay2[df$cigsPerDay[df$currentSmoker==1]==0] <- 'tranche 1'
df$cigsPerDay2[df$cigsPerDay[df$currentSmoker==1]<summary(df$cigsPerDay)[2]] <- 'tranche 2'
df$cigsPerDay2[df$cigsPerDay[df$currentSmoker==1]>=summary(df$cigsPerDay)[2] & df$cigsPerDay<=summary(df$cigsPerDay)[5]] <- 'tranche 3'
df$cigsPerDay2[df$cigsPerDay[df$currentSmoker==1]>summary(df$cigsPerDay)[5]] <- 'tranche 4'

# on echantillone
dfmalade <- df %>% filter(TenYearCHD == 'malade')
dfpasmalade <- df %>% filter(TenYearCHD == 'pas malade')
dfpasmalade2 <- dfpasmalade[sample(1:nrow(dfpasmalade),nrow(dfmalade)),]
df <- rbind(dfmalade, dfpasmalade2)

#  on defini df d'aprentissage et df test
set.seed(500)
split = sample.split(df$TenYearCHD, SplitRatio = 0.6666666666)

dfApprentissage = subset(df, split==TRUE)
dftest = subset(df, split==FALSE)

# regression logistique
modele.sature <- glm(formula = TenYearCHD~. , data = dfApprentissage, family = binomial)
summary(modele.sature)

# optimisation modele (step)
step(modele.sature)

monModele <- glm(formula = TenYearCHD ~ male + age + education + cigsPerDay + 
                   prevalentStroke + diabetes + totChol + sysBP, family = binomial, 
                 data = dfApprentissage)
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

appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5055, 1, 0)))
pourcentage <- (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$TenYearCHD)))
pourcentage

#calcul de pourcentage 
(pourcentage[1,1]+pourcentage[2,2])/sum(pourcentage)




result<-data.frame(matrix(0,50,6))
colnames(result)<-c("seuil","tx_global","PctgPop1","BienPred1","PctgPop0","BienPred0")
j=1
for(i in seq(from=0.2, to=0.9, by=0.01)){
  appren.p2 <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > i , 1, 0)))
  pourcentage <- (m.confusion <- as.matrix(table(appren.p2$pred.chd, appren.p2$TenYearCHD)))
  result[j,1]<-i
  result[j,2]<-(pourcentage[1,1]+pourcentage[2,2])/sum(pourcentage)
  result[j,3]<-(pourcentage[2,2])/(pourcentage[2,2]+pourcentage[1,2])
  result[j,4]<-(pourcentage[2,2])/(pourcentage[2,2]+pourcentage[2,1])
  result[j,5]<-(pourcentage[1,1])/(pourcentage[1,1]+pourcentage[2,1])
  result[j,6]<-(pourcentage[1,1])/(pourcentage[1,1]+pourcentage[1,2])
  
  j=j+1
}

library(ggplot2)
ggplot(result , aes(x=result$seuil , y = result$tx_global ))  + geom_line()
result <- result[1:15,]

p <- plot_ly(result, x = result$seuil, y = result$tx_global, name = 'trace 0', type = 'scatter', mode = 'lines') %>% 
add_trace(y = result$BienPred1, name = 'trace 1', mode = 'lines') %>% 
add_trace(y = result$BienPred0, name = 'trace 2', mode = 'lines')

p



# Courbe ROC
install.packages("ROCR")

pred <- prediction(appren.p$PredictedWin, appren.p$TenYearCHD)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des rÃ©sidus
res_dev <- residuals(modele.retenu) #residus de deviance
res_pear <- residuals(modele.retenu,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(modele.retenu) #residu de deviance standardises
H <- influence(modele.retenu)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(modele.retenu),type="p",cex=0.5,ylab="RÃ©sidus studentisÃ©s par VC")
abline(h=c(-2,2))

# PrÃ©vision
plot(predict(modele.retenu),rstudent(modele.retenu),type="p",cex=0.5,xlab="prÃ©vision linÃ©aire",
     ylab="RÃ©sidus studentisÃ©s par VC")

# Distance de cook
plot(cooks.distance(modele.retenu),type="h",ylab="Distance de Cook")


pred_test<-predict(modele.retenu,dftest,type = "response")
dftest2<-cbind(dftest,pred_test)

table(dftest$TenYearCHD)
table(dfApprentissage$TenYearCHD)
