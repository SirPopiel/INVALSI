setwd("~/GitHub/INVALSI/Data")

invalsi <- read.csv("cleaned_invalsi.csv")
excel <- read.csv2("excel.csv")

ita6 <- as.numeric(excel$ita6)
ita8 <- as.numeric(excel$ita8)
mat6 <- as.numeric(excel$mate6)
mat8 <- as.numeric(excel$mate8)

plot(ita6, ita8)

whole_data <- data.frame(ita6, ita8)

#install.packages("DataCombine")
library(DataCombine)
whole_data <- DropNA(whole_data)

plot(whole_data$ita6, whole_data$ita8, pch = 19, cex = 0.3)
abline(a=0,b=1, col = "red")

new_whole <- whole_data[which(whole_data$ita8>10 & whole_data$ita6 >10), ]
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3)

abline(a=0,b=1, col = "red")
fit <- lm(ita8 ~ ita6, data = new_whole)
summary(fit)
abline(fit, col = "steelblue1")

plot(fit)

library(splines)
model_cut = lm(ita8 ~ bs(ita6, knots = (seq(40,90,by=10)),degree=3), data = new_whole)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut, list(ita6=x.seq), se=T)
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1")
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2)
abline(a=0,b=1, col="red")

nord <- excel[which(excel$Nord==1),]
centro <- excel[which(excel$Centro==1),]
sud <- excel[which(excel$Sud==1),]

nord <- data.frame(ita6=nord$mate6, ita8=nord$mate8)
centro <- data.frame(ita6=centro$mate6, ita8=centro$mate8)
sud <- data.frame(ita6=sud$mate6, ita8=sud$mate8)

plot(nord, pch=19, cex=0.3, col='steelblue1')
points(centro, pch=19, cex=0.3, col = 'forestgreen')
points(sud, pch = 19, cex=0.3, col = 'red')

bk = c(40,90)
model_cut_nord = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = nord)
model_cut_centro = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = centro)
model_cut_sud = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = sud)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_nord, list(ita6=x.seq), se=T)
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3, col = 'white')
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_centro, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "blue", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "blue", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "blue", lty=2, lwd=2)
pred <- predict(model_cut_sud, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "navy", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "navy", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "navy", lty=2, lwd=2)

legend(8,100, legend = c("North", "Center", "South"), col = c("steelblue1", "blue", "navy"), lty=1, lwd=2, box.lty=0)

predN <- predict(model_cut_nord, list(ita6=x.seq), se=T)
predS <- predict(model_cut_sud, list(ita6=x.seq), se=T)

mean(predN$fit - predS$fit)

nord <- excel[which(excel$Nord==1),]
centro <- excel[which(excel$Centro==1),]
sud <- excel[which(excel$Sud==1),]

nord <- data.frame(ita6=nord$ita6, ita8=nord$ita8)
centro <- data.frame(ita6=centro$ita6, ita8=centro$ita8)
sud <- data.frame(ita6=sud$ita6, ita8=sud$ita8)

plot(nord, pch=19, cex=0.3, col='steelblue1')
points(centro, pch=19, cex=0.3, col = 'forestgreen')
points(sud, pch = 19, cex=0.3, col = 'red')

bk = c(40,90)
model_cut_nord = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = nord)
model_cut_centro = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = centro)
model_cut_sud = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = sud)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_nord, list(ita6=x.seq), se=T)
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3, col = 'white')
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_centro, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "blue", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "blue", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "blue", lty=2, lwd=2)
pred <- predict(model_cut_sud, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "navy", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "navy", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "navy", lty=2, lwd=2)

legend(8,100, legend = c("North", "Center", "South"), col = c("steelblue1", "blue", "navy"), lty=1, lwd=2, box.lty=0)

predN <- predict(model_cut_nord, list(ita6=x.seq), se=T)
predS <- predict(model_cut_sud, list(ita6=x.seq), se=T)

mean(predN$fit - predS$fit)

# Public vs Private

pubblica <- excel[which(excel$statale==1),]
paritaria <- excel[which(excel$paritaria==1),]
privata <- excel[which(excel$statale==0 & excel$paritaria==0),]

pubblica <- data.frame(ita6=pubblica$ita6, ita8=pubblica$ita8, mat6=pubblica$mate6, mat8=pubblica$mate8)
paritaria <- data.frame(ita6=paritaria$ita6, ita8=paritaria$ita8, mat6=paritaria$mate6, mat8=paritaria$mate8)
privata <- data.frame(ita6=privata$ita6, ita8=privata$ita8, mat6=privata$mate6, mat8=privata$mate8)

plot(pubblica[,3:4], pch=19, cex=0.3, col='steelblue1')
points(paritaria[,3:4], pch=19, cex=0.3, col = 'forestgreen')
points(privata[,3:4], pch = 19, cex=0.3, col = 'red')

bk = c(40,90)
model_cut_pubblica = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = pubblica)
model_cut_paritaria = lm(ita8 ~ ns(ita6, knots = (seq(60,80,by=10)), Boundary.knots = bk), data = paritaria)
model_cut_privata = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = privata)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_pubblica, list(ita6=x.seq), se=T)
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_paritaria, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "forestgreen", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "forestgreen", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "forestgreen", lty=2, lwd=2)
pred <- predict(model_cut_privata, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "red", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "red", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "red", lty=2, lwd=2)


pubblica <- excel[which(excel$public==1),]
privata <- excel[which(excel$public==0),]

pubblica <- data.frame(ita6=pubblica$ita6, ita8=pubblica$ita8, mat6=pubblica$mate6, mat8=pubblica$mate8)
privata <- data.frame(ita6=privata$ita6, ita8=privata$ita8, mat6=privata$mate6, mat8=privata$mate8)

plot(pubblica[,3:4], pch=19, cex=0.3, col='steelblue1')
points(privata[,3:4], pch = 19, cex=0.3, col = 'red')

library(splines)

bk = c(40,90)
model_cut_pubblica = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = pubblica)
model_cut_privata = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = privata)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_pubblica, list(mat6=x.seq), se=T)
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3, col='white', xlab='Maths 1st year', ylab = 'Maths 3rd year')
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_paritaria, list(ita6=x.seq), se=T)
pred <- predict(model_cut_privata, list(mat6=x.seq), se=T)
lines(x.seq, pred$fit, col = "orange", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "orange", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "orange", lty=2, lwd=2)

legend(8,100, legend = c("Public", "Private"), col = c("steelblue1", "orange"), lty=1, lwd = 2,box.lty=0)

maschi <- excel[which(excel$sesso=="Maschio"),]
femmine <- excel[which(excel$sesso=="Femmina"),]
maschi <- data.frame(ita6=maschi$ita6, ita8=maschi$ita8, mat6=maschi$mate6, mat8=maschi$mate8)
femmine <- data.frame(ita6=femmine$ita6, ita8=femmine$ita8, mat6=femmine$mate6, mat8=femmine$mate8)

plot(maschi[,1:2], pch=19, cex=0.3, col='steelblue1')
points(femmine[,1:2], pch=19, cex=0.3, col = 'violet')

maschi <- maschi[which(maschi$ita6>10, maschi$ita8>10),]
femmine <- femmine[which(femmine$ita6>10, femmine$ita8>10),]

bk = c(40,90)
model_cut_maschi = lm(ita8 ~ ns(ita6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = maschi)
model_cut_femmine = lm(ita8 ~ ns(ita6, knots = (seq(60,80,by=10)), Boundary.knots = bk), data = femmine)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_maschi, list(ita6=x.seq), se=T)
plot(new_whole$ita6, new_whole$ita8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_femmine, list(ita6=x.seq), se=T)
lines(x.seq, pred$fit, col = "violet", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "violet", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "violet", lty=2, lwd=2)

maschi <- excel[which(excel$sesso=="Maschio"),]
femmine <- excel[which(excel$sesso=="Femmina"),]
maschi <- data.frame(ita6=maschi$ita6, ita8=maschi$ita8, mat6=maschi$mate6, mat8=maschi$mate8)
femmine <- data.frame(ita6=femmine$ita6, ita8=femmine$ita8, mat6=femmine$mate6, mat8=femmine$mate8)

plot(maschi[,3:4], pch=19, cex=0.3, col='steelblue1')
points(femmine[,3:4], pch=19, cex=0.3, col = 'violet')

maschi <- maschi[which(maschi$mat6>10, maschi$mat8>10),]
femmine <- femmine[which(femmine$mat6>10, femmine$mat8>10),]

whole_data_m <- data.frame(mat6=excel$mate6, mat8=excel$mate8)

whole_data_m <- DropNA(whole_data_m)

plot(whole_data_m$mat6, whole_data_m$mat8, pch = 19, cex = 0.3)
abline(a=0,b=1, col = "red")

new_whole_m <- whole_data_m[which(whole_data_m$mat8>10 & whole_data_m$mat6 >10), ]
plot(new_whole_m$mat6, new_whole_m$mat8, pch = 19, cex = 0.3)

bk = c(40,90)
model_cut_maschi = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = maschi)
model_cut_femmine = lm(mat8 ~ ns(mat6, knots = (seq(60,80,by=10)), Boundary.knots = bk), data = femmine)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_maschi, list(mat6=x.seq), se=T)
plot(new_whole_m$mat6, new_whole_m$mat8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_femmine, list(mat6=x.seq), se=T)
lines(x.seq, pred$fit, col = "violet", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "violet", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "violet", lty=2, lwd=2)
abline(a=0,b=1, col="red", lwd=2)

# Let's try to undertand if there's some correlation between math and ita score

whole_data <- data.frame(mat8=excel$mate8, ita8=excel$ita8)

whole_data <- DropNA(whole_data)

plot(whole_data$ita8, whole_data$mat8, pch = 19, cex = 0.3)
abline(a=0,b=1, col = "red")

bk = c(40,90)
model_cut = lm(mat8 ~ ns(ita8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = whole_data)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut, list(ita8=x.seq), se=T)
plot(whole_data$ita8, whole_data$mat8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
abline(a=0,b=1, col="red", lwd=2)

bk = c(40,90)
model_cut = lm(ita8 ~ ns(mat8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = whole_data)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut, list(mat8=x.seq), se=T)
plot(whole_data$mat8, whole_data$ita8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
abline(a=0,b=1, col="red", lwd=2)

# Nord-Centro-Sud

nord <- excel[which(excel$Nord==1),]
centro <- excel[which(excel$Centro==1),]
sud <- excel[which(excel$Sud==1),]

nord <- data.frame(ita8=nord$ita8, mat8=nord$mate8)
centro <- data.frame(ita8=centro$ita8, mat8=centro$mate8)
sud <- data.frame(ita8=sud$ita8, mat8=sud$mate8)

plot(nord, pch=19, cex=0.3, col='steelblue1')
points(centro, pch=19, cex=0.3, col = 'forestgreen')
points(sud, pch = 19, cex=0.3, col = 'red')

bk = c(40,90)
model_cut_nord = lm(mat8 ~ ns(ita8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = nord)
model_cut_centro = lm(mat8 ~ ns(ita8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = centro)
model_cut_sud = lm(mat8 ~ ns(ita8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = sud)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_nord, list(ita8=x.seq), se=T)
plot(whole_data$ita8, whole_data$mat8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_centro, list(ita8=x.seq), se=T)
lines(x.seq, pred$fit, col = "forestgreen")
lines(x.seq, pred$fit + 2*pred$se.fit, col = "forestgreen", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "forestgreen", lty=2, lwd=2)
pred <- predict(model_cut_sud, list(ita8=x.seq), se=T)
lines(x.seq, pred$fit, col = "red", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "red", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "red", lty=2, lwd=2)


bk = c(40,90)
model_cut_nord = lm(ita8 ~ ns(mat8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = nord)
model_cut_centro = lm(ita8 ~ ns(mat8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = centro)
model_cut_sud = lm(ita8 ~ ns(mat8, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = sud)
x.seq = seq(10, 100, length.out=1000)
pred <- predict(model_cut_nord, list(mat8=x.seq), se=T)
plot(whole_data$mat8, whole_data$ita8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_centro, list(mat8=x.seq), se=T)
lines(x.seq, pred$fit, col = "forestgreen")
lines(x.seq, pred$fit + 2*pred$se.fit, col = "forestgreen", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "forestgreen", lty=2, lwd=2)
pred <- predict(model_cut_sud, list(mat8=x.seq), se=T)
lines(x.seq, pred$fit, col = "red", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "red", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "red", lty=2, lwd=2)

# What about the student's evaluation and the INVALSI score?

whole_data <- data.frame(mat8=excel$ita8, vot=excel$voto_orale_ita_cont)

whole_data <- DropNA(whole_data)

boxplot(whole_data$mat8[which(whole_data$vot %in% 3:10)]~factor(whole_data[which(whole_data$vot %in% 3:10),2], c(3,4,5,6,7,8,9,10)), xlab = "student's evaluation", ylab = "INVALSI score")

# Let's try to analyze significance with permutational tests

data <- data.frame(invalsi = whole_data$mat8[which(whole_data$vot %in% 3:10)], vot = factor(whole_data[which(whole_data$vot %in% 3:10),2]))

p.val <- numeric(7)

for(l in 1:7) {

  data_com <- data[which(data$vot %in% c((3+l-1),(3+l))),]
  fit <- aov(invalsi~vot, data=data_com)
  summary(fit)
  
  T0 <- summary(fit)[[1]][1,4]
  
  B = 10000
  
  Ti <- numeric(B)
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  for(i in 1:B){
    vot_perm <- sample(data_com$vot)
    fit_p <- aov(data_com$invalsi ~ vot_perm)
    Ti[i] <- summary(fit_p)[[1]][1,4]
    pb$tick()
  }
  
  plot(ecdf(Ti))
  
  p.val[l] <- sum(Ti >= T0)/B
  
}

# What about Nord - Centro - Sud?

whole_data <- data.frame(mat8=excel[which(excel$Nord==1),]$mate8, vot=excel[which(excel$Nord==1),]$voto_orale_mat_cont)

whole_data <- DropNA(whole_data)

boxplot(whole_data$mat8[which(whole_data$vot %in% 3:10)]~factor(whole_data[which(whole_data$vot %in% 3:10),2], c(3,4,5,6,7,8,9,10)), xlab = "student's evaluation", ylab = "INVALSI score")

data <- data.frame(invalsi = whole_data$mat8[which(whole_data$vot %in% 3:10)], vot = factor(whole_data[which(whole_data$vot %in% 3:10),2]))

p.valN <- numeric(6)

for(l in 1:6) {
  
  data_com <- data[which(data$vot %in% c((4+l-1),(4+l))),]
  fit <- aov(invalsi~vot, data=data_com)
  summary(fit)
  
  T0 <- summary(fit)[[1]][1,4]
  
  B = 10000
  
  Ti <- numeric(B)
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  for(i in 1:B){
    vot_perm <- sample(data_com$vot)
    fit_p <- aov(data_com$invalsi ~ vot_perm)
    Ti[i] <- summary(fit_p)[[1]][1,4]
    pb$tick()
  }
  
  plot(ecdf(Ti))
  
  p.valN[l] <- sum(Ti >= T0)/B
  
}



whole_data <- data.frame(mat8=excel[which(excel$Centro==1),]$mate8, vot=excel[which(excel$Centro==1),]$voto_orale_mat_cont)

whole_data <- DropNA(whole_data)

boxplot(whole_data$mat8[which(whole_data$vot %in% 3:10)]~factor(whole_data[which(whole_data$vot %in% 3:10),2], c(3,4,5,6,7,8,9,10)), xlab = "student's evaluation", ylab = "INVALSI score")


data <- data.frame(invalsi = whole_data$mat8[which(whole_data$vot %in% 3:10)], vot = factor(whole_data[which(whole_data$vot %in% 3:10),2]))

p.valC <- numeric(6)

for(l in 1:6) {
  
  data_com <- data[which(data$vot %in% c((4+l-1),(4+l))),]
  fit <- aov(invalsi~vot, data=data_com)
  summary(fit)
  
  T0 <- summary(fit)[[1]][1,4]
  
  B = 10000
  
  Ti <- numeric(B)
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  for(i in 1:B){
    vot_perm <- sample(data_com$vot)
    fit_p <- aov(data_com$invalsi ~ vot_perm)
    Ti[i] <- summary(fit_p)[[1]][1,4]
    pb$tick()
  }
  
  plot(ecdf(Ti))
  
  p.valC[l] <- sum(Ti >= T0)/B
  
}

whole_data <- data.frame(mat8=excel[which(excel$Sud==1),]$mate8, vot=excel[which(excel$Sud==1),]$voto_orale_mat_cont)

whole_data <- DropNA(whole_data)

boxplot(whole_data$mat8[which(whole_data$vot %in% 3:10)]~factor(whole_data[which(whole_data$vot %in% 3:10),2], c(3,4,5,6,7,8,9,10)), xlab = "student's evaluation", ylab = "INVALSI score")


data <- data.frame(invalsi = whole_data$mat8[which(whole_data$vot %in% 3:10)], vot = factor(whole_data[which(whole_data$vot %in% 3:10),2]))

p.valS <- numeric(7)

for(l in 1:7) {
  
  data_com <- data[which(data$vot %in% c((3+l-1),(3+l))),]
  fit <- aov(invalsi~vot, data=data_com)
  summary(fit)
  
  T0 <- summary(fit)[[1]][1,4]
  
  B = 10000
  
  Ti <- numeric(B)
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  for(i in 1:B){
    vot_perm <- sample(data_com$vot)
    fit_p <- aov(data_com$invalsi ~ vot_perm)
    Ti[i] <- summary(fit_p)[[1]][1,4]
    pb$tick()
  }
  
  plot(ecdf(Ti))
  
  p.valS[l] <- sum(Ti >= T0)/B
  
}


whole_data <- data.frame(mat8=excel[which(excel$statale==1),]$mate8, vot=excel[which(excel$statale==1),]$voto_orale_mat_cont)

whole_data <- DropNA(whole_data)

boxplot(whole_data$mat8[which(whole_data$vot %in% 3:10)]~factor(whole_data[which(whole_data$vot %in% 3:10),2], c(3,4,5,6,7,8,9,10)), xlab = "student's evaluation", ylab = "INVALSI score")


data <- data.frame(invalsi = whole_data$mat8[which(whole_data$vot %in% 3:10)], vot = factor(whole_data[which(whole_data$vot %in% 3:10),2]))

p.valstat <- numeric(6)

for(l in 1:6) {
  
  data_com <- data[which(data$vot %in% c((4+l-1),(4+l))),]
  fit <- aov(invalsi~vot, data=data_com)
  summary(fit)
  
  T0 <- summary(fit)[[1]][1,4]
  
  B = 10000
  
  Ti <- numeric(B)
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  for(i in 1:B){
    vot_perm <- sample(data_com$vot)
    fit_p <- aov(data_com$invalsi ~ vot_perm)
    Ti[i] <- summary(fit_p)[[1]][1,4]
    pb$tick()
  }
  
  plot(ecdf(Ti))
  
  p.valstat[l] <- sum(Ti >= T0)/B
  
}


whole_data <- data.frame(mat8=excel[which(excel$paritaria==1),]$mate8, vot=excel[which(excel$paritaria==1),]$voto_orale_mat_cont)

whole_data <- DropNA(whole_data)

boxplot(whole_data$mat8[which(whole_data$vot %in% 3:10)]~factor(whole_data[which(whole_data$vot %in% 3:10),2], c(3,4,5,6,7,8,9,10)), xlab = "student's evaluation", ylab = "INVALSI score")


data <- data.frame(invalsi = whole_data$mat8[which(whole_data$vot %in% 3:10)], vot = factor(whole_data[which(whole_data$vot %in% 3:10),2]))

p.valpar <- numeric(6)

for(l in 1:6) {
  
  data_com <- data[which(data$vot %in% c((4+l-1),(4+l))),]
  fit <- aov(invalsi~vot, data=data_com)
  summary(fit)
  
  T0 <- summary(fit)[[1]][1,4]
  
  B = 10000
  
  Ti <- numeric(B)
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  for(i in 1:B){
    vot_perm <- sample(data_com$vot)
    fit_p <- aov(data_com$invalsi ~ vot_perm)
    Ti[i] <- summary(fit_p)[[1]][1,4]
    pb$tick()
  }
  
  plot(ecdf(Ti))
  
  p.valpar[l] <- sum(Ti >= T0)/B
  
}


sudvsall <- data.frame(mat8=excel[which(excel$paritaria==1),]$mate8, vot=excel[which(excel$paritaria==1),]$voto_orale_mat_cont)
all <- data.frame(mat8=excel[which(excel$statale==1),]$mate8, vot=excel[which(excel$statale==1),]$voto_orale_mat_cont)

where <- c(rep("S", dim(sudvsall)[1]), rep("A", dim(all)[1]))

sudvsall <- rbind(sudvsall, all)

sudvsall$where = where

sudvsall <- sudvsall[which(sudvsall$vot==10),c(1,3)]
sudvsall <- DropNA(sudvsall)

fit <- aov(mat8 ~ factor(where), data=sudvsall)
summary(fit)

boxplot(sudvsall$mat8 ~ sudvsall$where)

sudvsall$where <- where

# New italians

whole_data <- data.frame(mat8=excel$mate8, mat6=excel$mate6)

itaboth <- whole_data[which(excel$luogo_madre=="Italia (o" & excel$luogo_padre=="Italia (o"),]
paritafor <- whole_data[which((excel$luogo_madre!="Italia (o" & excel$luogo_padre=="Italia (o") | (excel$luogo_madre=="Italia (o" & excel$luogo_padre!="Italia (o")),]
bothimm <- whole_data[which(excel$luogo_madre!="Italia (o" & excel$luogo_padre!="Italia (o"),]

itaboth <- DropNA(itaboth)
paritafor <- DropNA(paritafor)
bothimm <- DropNA(bothimm)
whole_data <- DropNA(whole_data)

plot(whole_data)

plot(bothimm)
plot(itaboth)
plot(paritafor)

library(splines)

bk = c(40,90)
model_cut_ita = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = itaboth)
model_cut_mix = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = paritafor)
model_cut_est = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = bothimm)
x.seq = seq(0, 100, length.out=1000)
pred <- predict(model_cut_ita, list(mat6=x.seq), se=T)
plot(whole_data$mat6, whole_data$mat8, pch = 19, cex = 0.3, col='white', xlab='Maths 1st year', ylab='Maths 3rd year')
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_mix, list(mat6=x.seq), se=T)
lines(x.seq, pred$fit, col = "blue", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "blue", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "blue", lty=2, lwd=2)
pred <- predict(model_cut_est, list(mat6=x.seq), se=T)
lines(x.seq, pred$fit, col = "navy", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "navy", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "navy", lty=2, lwd=2)


legend(0,100, legend = c("Ita-Ita", "Ita-For", "For-For"), col = c("steelblue1", "blue", "navy"), lty=1, lwd=2, box.lty=0, cex=0.7)
predI <- predict(model_cut_ita, list(mat6=x.seq[which(x.seq<50)]), se=T)
predF <- predict(model_cut_est, list(mat6=x.seq[which(x.seq<50)]), se=T)

mean(predI$fit - predF$fit)



# What about italian?

whole_data <- data.frame(mat8=excel$ita8, mat6=excel$ita6)

itaboth <- whole_data[which(excel$luogo_madre=="Italia (o" & excel$luogo_padre=="Italia (o"),]
paritafor <- whole_data[which((excel$luogo_madre!="Italia (o" & excel$luogo_padre=="Italia (o") | (excel$luogo_madre=="Italia (o" & excel$luogo_padre!="Italia (o")),]
bothimm <- whole_data[which(excel$luogo_madre!="Italia (o" & excel$luogo_padre!="Italia (o"),]

itaboth <- DropNA(itaboth)
paritafor <- DropNA(paritafor)
bothimm <- DropNA(bothimm)
whole_data <- DropNA(whole_data)

plot(whole_data)

plot(bothimm)
plot(itaboth)
plot(paritafor)

library(splines)

bk = c(40,90)
model_cut_ita = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = itaboth)
model_cut_mix = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=15)), Boundary.knots = bk), data = paritafor)
model_cut_est = lm(mat8 ~ ns(mat6, knots = (seq(50,80,by=10)), Boundary.knots = bk), data = bothimm)
x.seq = seq(0, 100, length.out=1000)
pred <- predict(model_cut_ita, list(mat6=x.seq), se=T)
plot(x.seq, pred$fit, col = "steelblue1", type='l', lwd=2, xlim = c(0,100), ylim=c(0,100))
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_mix, list(mat6=x.seq), se=T)
lines(x.seq, pred$fit, col = "blue", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "blue", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "blue", lty=2, lwd=2)
pred <- predict(model_cut_est, list(mat6=x.seq), se=T)
lines(x.seq, pred$fit, col = "navy", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "navy", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "navy", lty=2, lwd=2)

# Indaghiamo i punteggi di mate e ita in terza con una bella anova

whole_data <- data.frame(ita8=excel$ita8, mat8=excel$mate8)

ita <- whole_data[which(excel$luogo_madre=="Italia (o" & excel$luogo_padre=="Italia (o"),]
mix <- whole_data[which((excel$luogo_madre!="Italia (o" & excel$luogo_padre=="Italia (o") | (excel$luogo_madre=="Italia (o" & excel$luogo_padre!="Italia (o")),]
est <- whole_data[which(excel$luogo_madre!="Italia (o" & excel$luogo_padre!="Italia (o"),]

ita <- DropNA(ita)
mix <- DropNA(mix)
est <- DropNA(est)
whole_data <- DropNA(whole_data)

plot(ita, col = "blue", cex = 0.3, pch=19)
points(mix, col = "green", cex = 0.3, pch=19)
points(est, col = "red", cex = 0.3, pch=19)

plot(x = colMeans(ita)[1], y = colMeans(ita)[2], col = "blue", cex = 3, pch=3, xlab = "Ita", ylab = "Mat")
points(x = colMeans(mix)[1], y = colMeans(mix)[2], col = "green", cex = 3, pch=3)
points(x = colMeans(est)[1], y = colMeans(est)[2], col = "red", cex = 3, pch=3)


whole_data1 <- data.frame(ita8=excel$ita6, mat8=excel$mate6)

ita1 <- whole_data1[which(excel$luogo_madre=="Italia (o" & excel$luogo_padre=="Italia (o"),]
mix1 <- whole_data1[which((excel$luogo_madre!="Italia (o" & excel$luogo_padre=="Italia (o") | (excel$luogo_madre=="Italia (o" & excel$luogo_padre!="Italia (o")),]
est1 <- whole_data1[which(excel$luogo_madre!="Italia (o" & excel$luogo_padre!="Italia (o"),]

ita1 <- DropNA(ita1)
mix1 <- DropNA(mix1)
est1 <- DropNA(est1)
whole_data1 <- DropNA(whole_data1)

points(x = colMeans(ita1)[1], y = colMeans(ita1)[2], col = "blue", cex = 3, pch=4)
points(x = colMeans(mix1)[1], y = colMeans(mix1)[2], col = "green", cex = 3, pch=4)
points(x = colMeans(est1)[1], y = colMeans(est1)[2], col = "red", cex = 3, pch=4)


whole_data2 <- data.frame(ita8=excel$ita8, mat8=excel$mate8) - data.frame(ita8=excel$ita6, mat8=excel$mate6)

ita2 <- whole_data2[which(excel$luogo_madre=="Italia (o" & excel$luogo_padre=="Italia (o"),]
mix2 <- whole_data2[which((excel$luogo_madre!="Italia (o" & excel$luogo_padre=="Italia (o") | (excel$luogo_madre=="Italia (o" & excel$luogo_padre!="Italia (o")),]
est2 <- whole_data2[which(excel$luogo_madre!="Italia (o" & excel$luogo_padre!="Italia (o"),]

ita2 <- DropNA(ita2)
mix2 <- DropNA(mix2)
est2 <- DropNA(est2)
whole_data2 <- DropNA(whole_data2)

plot(x = colMeans(ita2)[1], y = colMeans(ita2)[2], col = "blue", cex = 3, pch=4, xlim=c(-4,0))
points(x = colMeans(mix2)[1], y = colMeans(mix2)[2], col = "green", cex = 3, pch=4)
points(x = colMeans(est2)[1], y = colMeans(est2)[2], col = "red", cex = 3, pch=4)

# Effect of escs

whole_data <- data.frame(escs = excel$ESCS, mat8 = excel$mate8)
plot(whole_data, pch = 19, cex=0.3)

library(splines)
model_cut = lm(mat8 ~ bs(escs, knots = (seq(40,90,by=10)),degree=3), data = whole_data)
x.seq = seq(-3, 2.7, length.out=1000)
pred <- predict(model_cut, list(escs=x.seq), se=T)
plot(whole_data$escs, whole_data$mat8, pch = 19, cex = 0.3, xlab='ESCS', ylab='Maths 3rd year')
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)

whole_data <- data.frame(escs = excel$ESCS, mat8 = excel$ita8)
plot(whole_data, pch = 19, cex=0.3)

library(splines)
model_cut = lm(mat8 ~ bs(escs, knots = (seq(40,90,by=10)),degree=3), data = whole_data)
x.seq = seq(-3, 2.7, length.out=1000)
pred <- predict(model_cut, list(escs=x.seq), se=T)
plot(whole_data$escs, whole_data$mat8, pch = 19, cex = 0.3, xlab='ESCS', ylab='Italian 3rd year')
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)

ECSC <- data.frame(excel$ESCS)
ECSC <- DropNA(ECSC)
quantiles <- quantile(ECSC, c(0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9))
quantiles

# Il numero di classi in una scuola può fare la differenza?

ct <- data.frame(excel$class_tot)
ct <- DropNA(ct)
quantile(ct)

whole_data <- data.frame(escs = excel$ESCS, mat8 = excel$mate8)
small <- whole_data[which(excel$class_tot < 18),]
normal <- whole_data[which(excel$class_tot >= 18),]


library(splines)
model_cut_small = lm(mat8 ~ bs(escs, knots = (seq(40,90,by=10)),degree=3), data = small)
model_cut_norm = lm(mat8 ~ bs(escs, knots = (seq(40,90,by=10)),degree=3), data = normal)
x.seq = seq(-3, 2.7, length.out=1000)
pred <- predict(model_cut_small, list(escs=x.seq), se=T)
plot(whole_data$escs, whole_data$mat8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_norm, list(escs=x.seq), se=T)
lines(x.seq, pred$fit, col = "red", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "red", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "red", lty=2, lwd=2)

whole_data <- data.frame(escs = excel$ESCS, mat8 = excel$mate6)
small <- whole_data[which(excel$class_tot < 18),]
normal <- whole_data[which(excel$class_tot >= 18),]

library(splines)
model_cut_small = lm(mat8 ~ ns(escs, df=3), data = small)
model_cut_norm = lm(mat8 ~ ns(escs, df=3), data = normal)
x.seq = seq(-3, 2.7, length.out=1000)
pred <- predict(model_cut_small, list(escs=x.seq), se=T)
plot(whole_data$escs, whole_data$mat8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_norm, list(escs=x.seq), se=T)
lines(x.seq, pred$fit, col = "red", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "red", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "red", lty=2, lwd=2)

ECSC <- data.frame(excel$ESCS)
ECSC <- DropNA(ECSC)
quantiles <- quantile(ECSC, c(0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9))
quantiles

# Bigger schools are better in overcoming inequalities

# What about public/private? Is an investment justified?


whole_data <- data.frame(escs = excel$ESCS, mat8 = excel$mate8)
public <- whole_data[which(excel$public == 1),]
private <- whole_data[which(excel$public == 0),]

library(splines)
model_cut_small = lm(mat8 ~ bs(escs, knots = (seq(40,90,by=10)),degree=3), data = public)
model_cut_norm = lm(mat8 ~ bs(escs, knots = (seq(40,90,by=10)),degree=3), data = private)
x.seq = seq(-3, 2.7, length.out=1000)
pred <- predict(model_cut_small, list(escs=x.seq), se=T)
plot(whole_data$escs, whole_data$mat8, pch = 19, cex = 0.3)
lines(x.seq, pred$fit, col = "steelblue1", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "steelblue1", lty=2, lwd=2)
pred <- predict(model_cut_norm, list(escs=x.seq), se=T)
lines(x.seq, pred$fit, col = "red", lwd=2)
lines(x.seq, pred$fit + 2*pred$se.fit, col = "red", lty=2, lwd=2)
lines(x.seq, pred$fit - 2*pred$se.fit, col = "red", lty=2, lwd=2)

# Not at all, there's no effect on inequalities

