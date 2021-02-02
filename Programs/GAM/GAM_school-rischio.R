# The goal of this notebooks is to use GAMs in order to predict the worst schools

setwd("~/GitHub/INVALSI/Data")
gpp <- read.csv("gppp.csv")

library(mgcv)
library(splines)
library(mgcViz)
library(DataCombine)
library(pROC)
library(splitTools)

set.seed(131997)

# Mate8 cutoff on means

data <- gpp[which(gpp$Fattore_correzione > 0.85),]

cutoff <- quantile(data$mate8_mean, 0.2)

data$rischio <- ifelse(data$mate8_mean < cutoff, 1, 0)

variables <- c('rischio', 'mate6_mean', 'Sud', 'condotta_studenti', 'prop_padre_ita', 'prop_madre_ita', 'prop_laurea_padre', 'prop_laurea_madre', 
                 'prop_padre_disocc', 'prop_madre_disocc', 'ESCS_mean', 'opinione_invalsi', 'utilizzo_invalsi', 'discussi_insegnanti', 'discussi_genitori',
                 'attivita_preside', 'pressioni_genitori', 'coinvolgimento_genitori_prop', 'coinvolgimento_genitori_eff', 'opinione_associazioni_genitori',
                  'public')

cols <- which(colnames(data) %in% variables)

data.gam <- data[,cols]

data.gam <- DropNA(data.gam)

gam.fit <- gam(rischio ~ s(mate6_mean, bs='cr') + cut(Sud, 2) + bs(prop_laurea_padre, degree=2) + bs(prop_laurea_madre, degree=2) + 
                 s(ESCS_mean, bs='cr') + bs(utilizzo_invalsi, df=3) + cut(discussi_insegnanti,2) +
                 bs(attivita_preside,df=5), data=data.gam, family='binomial', method='REML', select=T)


summary(gam.fit)

b <- getViz(gam.fit)

print(plot(b, allTerms = T), pages = 1)
print(plot(b, select=3))


# 4 fold cross validation to find the mean accuracy on test set

n = 49*2


par(mfrow = c(2,2))

data.gam <- data.gam[sample(dim(data.gam)[1]),]

cutoff = 0.2

accuracy = 0
sensitivity = 0
recall20 = 0

acc <- numeric(4)
sens <- numeric(4)
rec20 <- numeric(4)

tab_final <- matrix(0, nrow=2, ncol=2)
tab_final_2 <- matrix(0, nrow=2, ncol=2)

folds <- create_folds(data.gam$rischio, k = 4)  # Stratified 4-fold CV

for(i in 1:4) {
  data.gam.cv <- data.gam[folds[[i]],]
  gam.fit <- gam(rischio ~ s(mate6_mean, bs='cr') + cut(Sud, 2) + bs(prop_laurea_padre, degree=2) + bs(prop_laurea_madre, degree=2) + 
                   s(ESCS_mean, bs='cr') + bs(utilizzo_invalsi, df=3) + cut(discussi_insegnanti,2) +
                   bs(attivita_preside,df=5), data=data.gam.cv, family='binomial', method='REML', select=T)
  pred <- predict(gam.fit, data.gam[-folds[[i]],])
  rischiopred <- (1/(1+exp(-pred)) > cutoff)
  rischiopred_2 <- 1/(1+exp(-pred))
  rp2 <- (rischiopred_2 > quantile(rischiopred_2, 0.8))
  tab = table(data.gam$rischio[-folds[[i]]], rischiopred)
  tab2 = table(data.gam$rischio[-folds[[i]]], rp2)
  tab_final = tab_final + tab
  tab_final_2 = tab_final_2 + tab2
  acc[i] <- sum(diag(tab))/sum(tab)
  accuracy = accuracy + 1/4*sum(diag(tab))/sum(tab)
  sens[i] <- tab[2,2]/sum(tab[2,])
  sensitivity = sensitivity + 1/4*tab[2,2]/sum(tab[2,])
  recall20 <- recall20 + 1/4*tab_final_2[2,2]/sum(tab_final_2[2,])
  rec20[i] <- tab_final_2[2,2]/sum(tab_final_2[2,])
  print(tab)
  print(accuracy)
  print(sensitivity)
  
  pROC_obj <- roc(data.gam$rischio[-folds[[i]]], as.numeric(rischiopred),
                  smoothed = TRUE,
                  # arguments for ci
                  ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                  # arguments for plot
                  plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE)
  
  
  sens.ci <- ci.se(pROC_obj)
  plot(sens.ci, type="shape", col="lightblue")
  ## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
  ## definition shape.
  plot(sens.ci, type="bars")
}

tab_final
accuracy
sensitivity
recall20

sd(acc)
sd(sens)
sd(rec20)

# # # # # # # # # I T A - O T T O # # # # # # # # # #
#                                                   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# We repeat the exact same analysis for ita8

data <- gpp[which(gpp$Fattore_correzione > 0.85),]

cutoff <- quantile(data$ita8_mean, 0.2)

data$rischio <- ifelse(data$ita8_mean < cutoff, 1, 0)

variables <- c('rischio', 'ita6_mean', 'Sud', 'condotta_studenti', 'prop_padre_ita', 'prop_madre_ita', 'prop_laurea_padre', 'prop_laurea_madre', 
               'prop_padre_disocc', 'prop_madre_disocc', 'ESCS_mean', 'opinione_invalsi', 'utilizzo_invalsi', 'discussi_insegnanti', 'discussi_genitori',
               'attivita_preside', 'pressioni_genitori', 'coinvolgimento_genitori_prop', 'coinvolgimento_genitori_eff', 'opinione_associazioni_genitori',
               'public')
cols <- which(colnames(data) %in% variables)

data.gam <- data[,cols]

data.gam <- DropNA(data.gam)

gam.fit <- gam(rischio ~ bs(ita6_mean, degree=2) + cut(Sud, breaks = c(-Inf, 0.5, Inf)) + bs(prop_madre_ita, df=3) + bs(prop_laurea_padre, degree=3) + bs(prop_laurea_madre, degree=3) + 
                 s(ESCS_mean, bs='cr')  +
                 cut(pressioni_genitori, breaks = c(-Inf, 0.5, Inf)) + cut(coinvolgimento_genitori_prop, breaks=c(-Inf,1.111794,Inf)) + cut(coinvolgimento_genitori_eff, breaks=c(-Inf,1.5152330,Inf)) + cut(opinione_associazioni_genitori,breaks = c(-Inf, -0.5, 0.5, Inf)), data=data.gam, family='binomial', method='REML', select=T)



summary(gam.fit)

b <- getViz(gam.fit)

print(plot(b, allTerms = T), pages = 1)

# 4 fold cross validation to find the mean accuracy on test set

n = 49*2
accuracy = 0
sensitivity = 0
recall20 = 0

tab_final <- matrix(0, nrow=2, ncol=2)
tab_final_2 <- matrix(0, nrow=2, ncol=2)

cutoff = 0.2

par(mfrow = c(2,2))
plot(b, select=1)
plot(b, select=2)
plot(b, select=3)
plot(b, select=8)
plot(b, select=10)

folds <- create_folds(data.gam$rischio, k = 4)  # Stratified 4-fold CV

for(i in 1:4) {
  data.gam.cv <- data.gam[folds[[i]],]
  gam.fit <- gam(rischio ~ bs(ita6_mean, degree=2) + cut(Sud, breaks = c(-Inf, 0.5, Inf)) + bs(prop_madre_ita, df=3) + bs(prop_laurea_padre, degree=3) + bs(prop_laurea_madre, degree=3) + 
                   s(ESCS_mean, bs='cr')  +
                   cut(pressioni_genitori, breaks = c(-Inf, 0.5, Inf)) + cut(coinvolgimento_genitori_prop, breaks=c(-Inf,1.111794,Inf)) + cut(coinvolgimento_genitori_eff, breaks=c(-Inf,1.5152330,Inf)) + cut(opinione_associazioni_genitori,breaks = c(-Inf, -0.5, 0.5, Inf)), data=data.gam.cv, family='binomial', method='REML', select=T)
  
  #gam.fit <- gam(rischio ~ bs(ita6_mean, degree=2), data=data.gam.cv, family='binomial', method='REML', select=T)
  pred <- predict(gam.fit, data.gam[-folds[[i]],])
  rischiopred <- (1/(1+exp(-pred)) > cutoff)
  rischiopred_2 <- 1/(1+exp(-pred))
  rp2 <- (rischiopred_2 > quantile(rischiopred_2, 0.8))
  tab = table(data.gam$rischio[-folds[[i]]], rischiopred)
  tab2 = table(data.gam$rischio[-folds[[i]]], rp2)
  tab_final = tab_final + tab
  tab_final_2 = tab_final_2 + tab2
  acc[i] <- sum(diag(tab))/sum(tab)
  accuracy = accuracy + 1/4*sum(diag(tab))/sum(tab)
  sens[i] <- tab[2,2]/sum(tab[2,])
  sensitivity = sensitivity + 1/4*tab[2,2]/sum(tab[2,])
  recall20 <- recall20 + 1/4*tab_final_2[2,2]/sum(tab_final_2[2,])
  rec20[i] <- tab_final_2[2,2]/sum(tab_final_2[2,])
  print(tab)
  print(accuracy)
  print(sensitivity)
  
  pROC_obj <- roc(data.gam$rischio[-folds[[i]]], as.numeric(rischiopred),
                  smoothed = TRUE,
                  # arguments for ci
                  ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                  # arguments for plot
                  plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE)
  
  
  sens.ci <- ci.se(pROC_obj)
  plot(sens.ci, type="shape", col="lightblue")
  ## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
  ## definition shape.
  plot(sens.ci, type="bars")
}

tab_final
accuracy
sensitivity
recall20

sd(acc)
sd(sens)
sd(rec20)
