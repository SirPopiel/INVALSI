# This script extracts the best performing schools among the ones showing significant discrepancies from mean performances
#  both at an average level and student interaction level.

setwd("~/GitHub/INVALSI/Data")

invalsi <- read.csv("cleaned_invalsi.csv")
excel <- read.csv2("excel.csv")

ita6 <- as.numeric(excel$ita6)
ita8 <- as.numeric(excel$ita8)
mat6 <- as.numeric(excel$mate6)
mat8 <- as.numeric(excel$mate8)

ITA = FALSE

if(!ITA){
  data_mix <- excel[which(excel$Fattore_correzione > 0.85),]
} else {
  data_mix <- excel[which(excel$Fattore_correzione > 0.95),]
}

data_mix$cod_scu_anonimo <- factor(data_mix$cod_scu_anonimo)

if(ITA) {
  data_mix <- data.frame(mat8 = data_mix$ita8, mat6 = data_mix$ita6, sch = data_mix$cod_scu_anonimo)
} else {
  data_mix <- data.frame(mat8 = data_mix$mate8, mat6 = data_mix$mate6, sch = data_mix$cod_scu_anonimo)
}

library(DataCombine)
data_mix <- DropNA(data_mix)

model <- lm(mat8 ~ mat6 + sch + sch:mat6, data=data_mix)
sum <- summary(model)
sum

if(!ITA) {
  N_schools <- 967
} else {
  N_schools <- 886
}

sum_schools <- sum$coefficients[3:N_schools,]
sum_stud_eff <- sum$coefficients[(N_schools+1):dim(sum$coefficients)[1],]

sum_schools <- sum_schools[which(sum_schools[,4] < 0.01),]
sum_stud_eff <- sum_stud_eff[which(sum_stud_eff[,4] < 0.01),]

q10_sch <- quantile(sum_schools[,1], 0.2)
q90_sch <- quantile(sum_schools[,1], 0.8)

q10_stu <- quantile(sum_stud_eff[,1], 0.2)
q90_stu <- quantile(sum_stud_eff[,1], 0.8)

schools_10 <- rownames(sum_schools)[which(sum_schools[,1] < q10_sch)]
schools_90 <- rownames(sum_schools)[which(sum_schools[,1] > q90_sch)]

stud_10 <- rownames(sum_stud_eff)[which(sum_stud_eff[,1] < q10_stu)]
stud_90 <- rownames(sum_stud_eff)[which(sum_stud_eff[,1] > q90_stu)]

stud_10
stud_90
schools_10
schools_90

gpp <- read.csv("gpppp.csv")

# Purifying gpp

data <- data.frame(mate6mean = gpp$X..mate6....mean..)
data$mate6median <- gpp$X..mate6....median..
data$mate6max <- gpp$X..mate6....max..
data$mate6min <- gpp$X..mate6....min..
data$mate6q25 <- gpp$X..mate6.....lambda_0...
data$mate6q75 <- gpp$X..mate6.....lambda_1...
data$mate6std <- gpp$X..mate6....std..
data$mate6ske <- gpp$X..mate6....skew..

data$mate8mean <- gpp$X..mate8....mean..
data$mate8median <- gpp$X..mate8....median..
data$mate8max <- gpp$X..mate8....max..
data$mate8min <- gpp$X..mate8....min..
data$mate8q25 <- gpp$X..mate8.....lambda_0...
data$mate8q75 <- gpp$X..mate8.....lambda_1...
data$mate8std <- gpp$X..mate8....std..
data$mate8ske <- gpp$X..mate8....skew..

data$ita6mean <- gpp$X..ita6....mean..
data$ita6median <- gpp$X..ita6....median..
data$ita6max <- gpp$X..ita6....max..
data$ita6min <- gpp$X..ita6....min..
data$ita6q25 <- gpp$X..ita6.....lambda_0...
data$ita6q75 <- gpp$X..ita6.....lambda_1...
data$ita6std <- gpp$X..ita6....std..
data$ita6ske <- gpp$X..ita6....skew..

data$ita8mean <- gpp$X..ita8....mean..
data$ita8median <- gpp$X..ita8....median..
data$ita8max <- gpp$X..ita8....max..
data$ita8min <- gpp$X..ita8....min..
data$ita8q25 <- gpp$X..ita8.....lambda_0...
data$ita8q75 <- gpp$X..ita8.....lambda_1...
data$ita8std <- gpp$X..ita8....std..
data$ita8ske <- gpp$X..ita8....skew..

data$deltastdmate <- data$mate8std - data$mate6std
data$deltastdita <- data$ita8std - data$ita6std

data$nord = gpp$Nord
data$centro = gpp$Centro
data$sud = gpp$Sud

data$pon = ifelse(gpp$Pon == "Area_Pon", 1, 0)

data$statale = gpp$statale
data$paritaria = gpp$paritaria
data$privata = (1-gpp$public)

data$ESCSmean = gpp$X..ESCS....mean..
data$ESCSmedian = gpp$X..ESCS....median..
data$ESCSmin = gpp$X..ESCS....min..
data$ESCSmax = gpp$X..ESCS....max..
data$ESCSq25 = gpp$X..ESCS.....lambda_0...
data$ESCSq75 = gpp$X..ESCS....lambda_1...
data$ESCSstd = gpp$X..ESCS....std..
data$ESCSske = gpp$X..ESCS....skew..

data$correzione = gpp$Fattore_correzione

data$op_invalsi = gpp$opinione_invalsi
data$ut_invalsi = gpp$utilizzo_invalsi
data$sod_pon = gpp$sodd_pon
data$att_pres = gpp$attivita_preside
data$coinv_g_pr = gpp$coinvolgimento_genitori_prop
data$coinv_g_ef = gpp$coinvolgimento_genitori_eff
data$cond = gpp$condotta_studenti
data$infr = gpp$infrastrutture
data$str = gpp$strumenti

data$maschi <- gpp$prop_maschi
data$boc <- gpp$prop_bocciati
data$cultpa <- gpp$prop_diploma_padre + gpp$prop_laurea_padre
data$cultma <- gpp$prop_diploma_madre + gpp$prop_laurea_madre
data$imm1 <- gpp$prop_imm1
data$imm2 <- gpp$prop_imm2
data$disins <-gpp$discussi_insegnanti
data$disgen <- gpp$discussi_genitori
data$presgen <- gpp$pressioni_genitori
data$opgen <- gpp$opinione_associazioni_genitori
data$regel <- gpp$registro_elettronico
data$imtot <- gpp$prop_imm1 + gpp$prop_imm2

data$where = ifelse(data$sud == 1, 0, ifelse(data$nord == 1, 1, 0.5))

data$cod_scu = as.character(gpp$cod_scu_anonimo)

student_level = FALSE

cods <- c()
for(i in 1:length(schools_10)) {
  if(!student_level) {
    cods <- c(cods, sub("sch", "", schools_10[i]))
  }
  if(student_level){
    cods_part <- c(cods, sub("mat6:", "", schools_10[i]))
    cods <- c(cods, sub("sch", "", schools_10[i]))
  }
}

data_sch_flop <- data[which(data$cod_scu %in% cods),]

cods <- c()
for(i in 1:length(schools_90)) {
  if(!student_level) {
    cods <- c(cods, sub("sch", "", schools_90[i]))
  }
  if(student_level){
    cods_part <- c(cods, sub("mat6:", "", schools_90[i]))
    cods <- c(cods, sub("sch", "", schools_90[i]))
  }
}

data_sch_top <- data[which(data$cod_scu %in% cods),]

mean(data_sch_top$cond, na.rm = T)
mean(data_sch_flop$cond, na.rm = T)

# Apply some permutational tests to check whether there are some differences in the 2 parts:

colnames(data_sch_top)

idxs <- 27:61
n_idx <- length(idxs)

p.vals <- rep(NA,n_idx)

for(i in 1:n_idx) {
  T0 <- mean(data_sch_top[,idxs[i]], na.rm=T)-mean(data_sch_flop[,idxs[i]], na.rm=T)
  print(T0)
}

for(i in 1:n_idx) {
  T0 <- abs(mean(data_sch_top[,idxs[i]], na.rm=T)-mean(data_sch_flop[,idxs[i]], na.rm=T))
  vec <- c(data_sch_top[,idxs[i]], data_sch_flop[,idxs[i]])
  ntop <- dim(data_sch_top)[1]
  nflop <- dim(data_sch_flop)[1]
  B <- 5000
  Ti <- rep(NA,B)
  for(j in 1:B) {
    sam <- sample(1:(ntop+nflop), ntop)
    Ti[j] <- abs(mean(vec[sam], na.rm=T) - mean(vec[-sam], na.rm=T))
  }
  p.vals[i] = sum(Ti >= T0)/B
}

p.vals[which(p.vals < 0.1)]
colnames(data_sch_top)[which(p.vals < 0.1) + 26]


colnames(data_sch_top)

idxs <- 27:61
n_idx <- length(idxs)

p.vals <- rep(NA,n_idx)


for(i in 1:n_idx) {
  T0 <- sd(data_sch_top[,idxs[i]], na.rm=T)-sd(data_sch_flop[,idxs[i]], na.rm=T)
  print(T0)
}

for(i in 1:n_idx) {
  T0 <- abs(sd(data_sch_top[,idxs[i]], na.rm=T)-sd(data_sch_flop[,idxs[i]], na.rm=T))
  vec <- c(data_sch_top[,idxs[i]], data_sch_flop[,idxs[i]])
  ntop <- dim(data_sch_top)[1]
  nflop <- dim(data_sch_flop)[1]
  B <- 5000
  Ti <- rep(NA,B)
  for(j in 1:B) {
    sam <- sample(1:(ntop+nflop), ntop)
    Ti[j] <- abs(sd(vec[sam], na.rm=T) - sd(vec[-sam], na.rm=T))
  }
  p.vals[i] = sum(Ti >= T0)/B
}

p.vals
colnames(data_sch_top)[which(p.vals < 0.1) + 26]
