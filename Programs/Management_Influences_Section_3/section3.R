#' Code for Section 3
#' Lines 9-11: general imports
#' Lines 16-1153: 260 questions
#' Lines 1154-1325: Gathering the 29 results
#' Lines 1326-end: Stress testing the 29 results
#'
#'
#' @author F. Capello 



rm(list=ls())
source("./utils.R")
source("./prepare_data.R")



# Q1: studenti pi?? educati migliorano anche di pi??, in media, in italiano? 
hist(df$condotta_studenti)
bootstrapReg(df$condotta_studenti, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q2: studenti pi?? educati migliorano anche di pi??, in media, in matematica? 
hist(df$condotta_studenti)
bootstrapReg(df$condotta_studenti, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)


# Q3: studenti pi?? educati hanno punteggi pi?? alti, in media, in italiano? 
hist(df$ita8_mean)
bootstrapReg(df$condotta_studenti, df$ita8_mean, alpha=0.01, B=2e3)

# Q4: studenti pi?? educati hanno punteggi pi?? alti, in media, in matematica? 
hist(df$mate8_mean)
bootstrapReg(df$condotta_studenti, df$mate8_mean, alpha=0.01, B=2e3)


# Q5: * l'attivit?? dei presidi impatta sui punteggi, in media, in italiano?
hist(df$attivita_preside)
bootstrapReg(df$attivita_preside, df$ita8_mean, alpha=0.01, B=2e3)

# Q6: * l'attivit?? dei presidi impatta sui punteggi, in media, in matematica?
bootstrapReg(df$attivita_preside, df$mate8_mean, alpha=0.01, B=2e3)


# Q7: l'attivit?? dei presidi impatta sui miglioramenti, in media, in italiano?
bootstrapReg(df$attivita_preside, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)


# Q8: l'attivit?? dei presidi impatta sui miglioramenti, in media, in matematica?
bootstrapReg(df$attivita_preside, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)


# Q9: La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di italiano?
hist(df$pressioni_genitori)
boxplot(df$ita8_mean ~ df$pressioni_genitori)
uniPermTest(df$ita8_mean, df$pressioni_genitori, B=1e4)

# Q10: La pressione dei genitori impatta sui punteggi, in media, in matematica?
boxplot(df$mate8_mean ~ df$pressioni_genitori)
uniPermTest(df$ita8_mean, df$pressioni_genitori, B=1e4)

# Q11: La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano?
boxplot(df$ita8_mean ~ df$pressioni_genitori)
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$pressioni_genitori, B=1e4)

# Q12: La pressione dei genitori, percepita dai presidi, ha impatto sui miglioramenti in matematica?
boxplot(df$improv_mate_rapp_mean ~ df$pressioni_genitori)
uniPermTest(df$improv_mate_rapp_mean, df$pressioni_genitori, B=1e4)

# Q13: La pressione dei genitori, percepita dai presidi, ha impatto sui miglioramenti in italiano?
boxplot(df$improv_ita_rapp_mean ~ df$pressioni_genitori)
uniPermTest(df$improv_ita_rapp_mean, df$pressioni_genitori, B=1e4)


# Q14: il coinvolgimento dei genitori impatta sui voti, in media, in italiano?
hist(df$coinvolgimento_genitori_eff)
bootstrapReg(df$coinvolgimento_genitori_eff, df$ita8_mean, alpha=0.01, B=2e3)

# Q15: il coinvolgimento dei genitori impatta sui voti, in media, in matematica?
bootstrapReg(df$coinvolgimento_genitori_eff, df$mate8_mean, alpha=0.01, B=2e3)

# Q16: * il coinvolgimento dei genitori impatta sui voti, in media, in italiano + matematica?
df$ita8_plus_mate8_mean = df$mate8_mean + df$ita8_mean
bootstrapReg(df$coinvolgimento_genitori_eff, df$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q17: il coinvolgimento dei genitori impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(df$coinvolgimento_genitori_eff, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q18: il coinvolgimento dei genitori impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(df$coinvolgimento_genitori_eff, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q19: * L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di italiano?
df$opinione_associazioni_genitori_binary = as.integer(df$opinione_associazioni_genitori > 0)
hist(df$opinione_associazioni_genitori_binary)
boxplot(df$ita8_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$ita8_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# Q20: * L'opinione sulle associazioni di genitori impatta sui punteggi, in media, in matematica?
boxplot(df$mate8_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$mate8_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# Q21: * L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$opinione_associazioni_genitori_binary, B=1e4)
plot(df$ita8_mean, df$mate8_mean, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# Q22: L'opinione delle associazioni di genitori, percepita dai presidi, ha impatto sui miglioramenti in matematica?
boxplot(df$improv_mate_rapp_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$improv_mate_rapp_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# Q23: L'opinione delle associazioni di genitori, percepita dai presidi, ha impatto sui miglioramenti in italiano?
boxplot(df$improv_ita_rapp_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$improv_ita_rapp_mean, df$opinione_associazioni_genitori_binary, B=1e4)





# Q24: Le infrastrutture impattano, in media, in italiano?
hist(df$infrastrutture)
bootstrapReg(df$infrastrutture, df$ita8_mean, alpha=0.01, B=2e3)

# Q25: Le infrastrutture impattano, in media, in italiano?
bootstrapReg(df$infrastrutture, df$mate8_mean, alpha=0.01, B=2e3)

# Q26: Le infrastrutture impattano, in media, in italiano + matematica?
bootstrapReg(df$infrastrutture, df$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q27: Le infrastrutture impattano sul miglioramento dei voti, in media, in italiano?
bootstrapReg(df$infrastrutture, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q28: Le infrastrutture impattano sul miglioramento dei voti, in media, in matematica?
bootstrapReg(df$infrastrutture, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)



# Q29: L'eta' preside impatta, in media, in italiano?
hist(df$eta_preside)
bootstrapReg(df$eta_preside, df$ita8_mean, alpha=0.01, B=2e3)

# Q30: L'eta' preside impatta, in media, in italiano?
bootstrapReg(df$eta_preside, df$mate8_mean, alpha=0.01, B=2e3)

# Q31: L'eta' preside impatta, in media, in italiano + matematica?
bootstrapReg(df$eta_preside, df$ita8_plus_mate8_mean, alpha=0.01, B=3e3)

# Q32: L'eta' preside impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(df$eta_preside, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q33: L'eta' preside impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(df$eta_preside, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q34: Preside maschio ha impatto sui punteggi di italiano?
hist(df$preside_maschio)
boxplot(df$ita8_mean ~ df$preside_maschio)
uniPermTest(df$ita8_mean, df$preside_maschio, B=1e4)

# Q35: Preside maschio impatta sui punteggi, in media, in matematica?
boxplot(df$mate8_mean ~ df$preside_maschio)
uniPermTest(df$mate8_mean, df$preside_maschio, B=1e4)

# Q36: Preside maschio ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$preside_maschio, B=1e4)
plot(df$ita8_mean, df$mate8_mean, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# Q37: Preside maschio ha impatto sui miglioramenti in matematica?
boxplot(df$improv_mate_rapp_mean ~ df$preside_maschio)
uniPermTest(df$improv_mate_rapp_mean, df$preside_maschio, B=1e4)

# Q38: Preside maschio ha impatto sui miglioramenti in italiano?
boxplot(df$improv_ita_rapp_mean ~ df$preside_maschio)
uniPermTest(df$improv_ita_rapp_mean, df$preside_maschio, B=1e4)



# Q39: L'utilizzo invalsi impatta, in media, in italiano?
hist(df$utilizzo_invalsi)
bootstrapReg(df$utilizzo_invalsi, df$ita8_mean, alpha=0.01, B=2e3)

# Q40: L'utilizzo invalsi impatta, in media, in italiano?
bootstrapReg(df$utilizzo_invalsi, df$mate8_mean, alpha=0.01, B=2e3)

# Q41: L'utilizzo invalsi impatta, in media, in italiano + matematica?
bootstrapReg(df$utilizzo_invalsi, df$ita8_plus_mate8_mean, alpha=0.01, B=3e3)

# Q42: L'utilizzo invalsi impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(df$utilizzo_invalsi, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q43: L'utilizzo invalsi impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(df$utilizzo_invalsi, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q44: il coinvolgimento prop dei genitori impatta sui voti, in media, in italiano?
hist(df$coinvolgimento_genitori_prop)
bootstrapReg(df$coinvolgimento_genitori_prop, df$ita8_mean, alpha=0.01, B=2e3)

# Q45: il coinvolgimento prop dei genitori impatta sui voti, in media, in italiano?
bootstrapReg(df$coinvolgimento_genitori_prop, df$mate8_mean, alpha=0.01, B=2e3)

# Q46: il coinvolgimento prop dei genitori impatta sui voti, in media, in italiano + matematica?
bootstrapReg(df$coinvolgimento_genitori_prop, df$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q47: il coinvolgimento prop dei genitori impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(df$coinvolgimento_genitori_prop, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q48: il coinvolgimento prop dei genitori impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(df$coinvolgimento_genitori_prop, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q49: La presenza di strumenti impatta sui voti, in media, in italiano?
hist(df$strumenti)
bootstrapReg(df$strumenti, df$ita8_mean, alpha=0.01, B=2e3)

# Q50: La presenza di strumenti impatta sui voti, in media, in italiano?
bootstrapReg(df$strumenti, df$mate8_mean, alpha=0.01, B=2e3)

# Q51: La presenza di strumenti impatta sui voti, in media, in italiano + matematica?
bootstrapReg(df$strumenti, df$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q52: La presenza di strumenti impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(df$strumenti, df$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q53: La presenza di strumenti impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(df$strumenti, df$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q54: Strumenti <> 1.1 ha impatto sui punteggi di italiano?
df$strumenti_binary = as.integer(df$strumenti > 1.1)
hist(df$strumenti_binary)
boxplot(df$ita8_mean ~ df$strumenti_binary)
uniPermTest(df$ita8_mean, df$strumenti_binary, B=1e4)

# Q55: Strumenti <> 1.1 impatta sui punteggi, in media, in matematica?
boxplot(df$mate8_mean ~ df$strumenti_binary)
uniPermTest(df$mate8_mean, df$strumenti_binary, B=1e4)

# Q56: Strumenti <> 1.1 ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$strumenti_binary, B=1e4)
plot(df$ita8_mean, df$mate8_mean, col=df$strumenti_binary+3, pch='*')

# Q57: Strumenti <> 1.1 ha impatto sui miglioramenti in matematica?
boxplot(df$improv_mate_rapp_mean ~ df$strumenti_binary)
uniPermTest(df$improv_mate_rapp_mean, df$strumenti_binary, B=1e4)

# Q58: Strumenti <> 1.1 ha impatto sui miglioramenti in italiano?
boxplot(df$improv_ita_rapp_mean ~ df$strumenti_binary)
uniPermTest(df$improv_ita_rapp_mean, df$strumenti_binary, B=1e4)





# # # # # # And now, Standard Deviation!! # # # # # #
#                                                   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #


# Q1: studenti pi?? educati hanno pi?? varianza di miglioramento, in media, in italiano? 
hist(df$condotta_studenti)
bootstrapReg(df$condotta_studenti, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q2: studenti pi?? educati hanno pi?? varianza di miglioramento, in media, in matematica? 
bootstrapReg(df$condotta_studenti, df$improv_mate_rapp_std, alpha=0.01, B=2e3)


# Q3: studenti pi?? educati hanno varianza pi?? alta, in media, in italiano? 
hist(df$ita8_std)
bootstrapReg(df$condotta_studenti, df$ita8_std, alpha=0.01, B=2e3)

# Q4: studenti pi?? educati hanno vairanza pi?? alta, in media, in matematica? 
hist(df$mate8_std)
bootstrapReg(df$condotta_studenti, df$mate8_std, alpha=0.01, B=2e3)



# Q5: l'attivit?? dei presidi impatta su varianza di punteggi, in media, in italiano?
hist(df$attivita_preside)
bootstrapReg(df$attivita_preside, df$ita8_std, alpha=0.01, B=4e3)

# Q6: * l'attivit?? dei presidi impatta su varianza punteggi, in media, in matematica?
bootstrapReg(df$attivita_preside, df$mate8_std, alpha=0.01, B=2e3)

# Q7: l'attivit?? dei presidi impatta su varianza miglioramenti, in media, in italiano?
bootstrapReg(df$attivita_preside, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q8: l'attivit?? dei presidi impatta su varianza miglioramenti, in media, in matematica?
bootstrapReg(df$attivita_preside, df$improv_mate_rapp_std, alpha=0.01, B=2e3)


# Q9: La pressione dei genitori, percepita dai presidi, ha impatto su varianza punteggi di italiano?
hist(df$pressioni_genitori)
boxplot(df$ita8_std ~ df$pressioni_genitori)
uniPermTest(df$ita8_std, df$pressioni_genitori, B=1e4)

# Q10: La pressione dei genitori impatta su varianza punteggi, in media, in matematica?
boxplot(df$mate8_std ~ df$pressioni_genitori)
uniPermTest(df$ita8_std, df$pressioni_genitori, B=1e4)

# Q11: * La pressione dei genitori, percepita dai presidi, ha impatto su varianza punteggi di matematica e italiano?
boxplot(df$ita8_std ~ df$pressioni_genitori)
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$pressioni_genitori, B=1e4)
plot(df$ita8_std, df$mate8_std, col=df$pressioni_genitori + 1, pch="*")

# Q12: La pressione dei genitori, percepita dai presidi, ha impatto su varianza miglioramenti in matematica?
boxplot(df$improv_mate_rapp_std ~ df$pressioni_genitori)
uniPermTest(df$improv_mate_rapp_std, df$pressioni_genitori, B=1e4)

# Q13: La pressione dei genitori, percepita dai presidi, ha impatto sui varianza miglioramenti in italiano?
boxplot(df$improv_ita_rapp_std ~ df$pressioni_genitori)
uniPermTest(df$improv_ita_rapp_std, df$pressioni_genitori, B=1e4)



# Q14: il coinvolgimento dei genitori impatta sulla varianza dei voti, in media, in italiano?
hist(df$coinvolgimento_genitori_eff)
bootstrapReg(df$coinvolgimento_genitori_eff, df$ita8_std, alpha=0.01, B=2e3)

# Q15: il coinvolgimento dei genitori impatta sulla varianza voti, in media, in italiano?
bootstrapReg(df$coinvolgimento_genitori_eff, df$mate8_std, alpha=0.01, B=2e3)

# Q16: il coinvolgimento dei genitori impatta sulla varianza voti, in media, in italiano + matematica?
df$ita8_plus_mate8_std = df$mate8_std + df$ita8_std
bootstrapReg(df$coinvolgimento_genitori_eff, df$ita8_plus_mate8_std, alpha=0.01, B=3e3)

# Q17: il coinvolgimento dei genitori impatta su varianza miglioramento dei voti, in media, in italiano?
bootstrapReg(df$coinvolgimento_genitori_eff, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q18: il coinvolgimento dei genitori impatta su varianza miglioramento dei voti, in media, in matematica?
bootstrapReg(df$coinvolgimento_genitori_eff, df$improv_mate_rapp_std, alpha=0.01, B=2e3)




# Q19: L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto su varianza punteggi di italiano?
hist(df$opinione_associazioni_genitori_binary)
boxplot(df$ita8_std ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$ita8_std, df$opinione_associazioni_genitori_binary, B=1e4)

# Q20: L'opinione sulle associazioni di genitori impatta su varianza punteggi, in media, in matematica?
boxplot(df$mate8_std ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$mate8_std, df$opinione_associazioni_genitori_binary, B=1e4)

# Q21: L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto su varianza punteggi di matematica e italiano?
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$opinione_associazioni_genitori_binary, B=1e4)

# Q22: * L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto su varianza miglioramenti in matematica?
boxplot(df$improv_mate_rapp_std ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$improv_mate_rapp_std, df$opinione_associazioni_genitori_binary, B=1e4)

# Q23: L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto su varianza miglioramenti in italiano?
boxplot(df$improv_ita_rapp_std ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$improv_ita_rapp_std, df$opinione_associazioni_genitori_binary, B=1e4)





# Q24: Le infrastrutture impattano, in media, su varianza italiano?
hist(df$infrastrutture)
bootstrapReg(df$infrastrutture, df$ita8_std, alpha=0.01, B=2e3)

# Q25: Le infrastrutture impattano, in media, su varianza italiano?
bootstrapReg(df$infrastrutture, df$mate8_std, alpha=0.01, B=2e3)

# Q26: Le infrastrutture impattano, in media, su varianza italiano + matematica?
bootstrapReg(df$infrastrutture, df$ita8_plus_mate8_std, alpha=0.01, B=3e3)

# Q27: Le infrastrutture impattano sul miglioramento dei voti, in media, su varianza italiano?
bootstrapReg(df$infrastrutture, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q28: Le infrastrutture impattano sul miglioramento dei voti, in media, su varianza matematica?
bootstrapReg(df$infrastrutture, df$improv_mate_rapp_std, alpha=0.01, B=2e3)



# Q29: L'eta' preside impatta, in media, su varianza italiano?
hist(df$eta_preside)
bootstrapReg(df$eta_preside, df$ita8_std, alpha=0.01, B=2e3)

# Q30: L'eta' preside impatta, in media, su varianza italiano?
bootstrapReg(df$eta_preside, df$mate8_std, alpha=0.01, B=2e3)

# Q31: L'eta' preside impatta, in media, su varianza italiano + matematica?
bootstrapReg(df$eta_preside, df$ita8_plus_mate8_std, alpha=0.01, B=3e3)

# Q32: L'eta' preside impatta su varianza miglioramento dei voti, in media, in italiano?
bootstrapReg(df$eta_preside, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q33: L'eta' preside impatta su varianza miglioramento dei voti, in media, in matematica?
bootstrapReg(df$eta_preside, df$improv_mate_rapp_std, alpha=0.01, B=2e3)




# Q34: * Preside maschio ha impatto su varianza punteggi di italiano?
hist(df$preside_maschio)
boxplot(df$ita8_std ~ df$preside_maschio)
uniPermTest(df$ita8_std, df$preside_maschio, B=1e4)

# Q35: * Preside maschio impatta sui punteggi, in media, in matematica?
boxplot(df$mate8_std ~ df$preside_maschio)
uniPermTest(df$mate8_std, df$preside_maschio, B=2e4)

# Q36: * Preside maschio ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$preside_maschio, B=1e4)
plot(df$ita8_std, df$mate8_std, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# Q37: Preside maschio ha impatto su varianza miglioramenti in matematica?
boxplot(df$improv_mate_rapp_std ~ df$preside_maschio)
uniPermTest(df$improv_mate_rapp_std, df$preside_maschio, B=1e4)

# Q38: Preside maschio ha impatto su varianza miglioramenti in italiano?
boxplot(df$improv_ita_rapp_std ~ df$preside_maschio)
uniPermTest(df$improv_ita_rapp_std, df$preside_maschio, B=1e4)



# Q39: L'utilizzo invalsi impatta, in media, su varianza italiano?
hist(df$utilizzo_invalsi)
bootstrapReg(df$utilizzo_invalsi, df$ita8_std, alpha=0.01, B=2e3)

# Q40: L'utilizzo invalsi impatta, in media, su varianza italiano?
bootstrapReg(df$utilizzo_invalsi, df$mate8_std, alpha=0.01, B=2e3)

# Q41: L'utilizzo invalsi impatta, in media, su varianza italiano + matematica?
bootstrapReg(df$utilizzo_invalsi, df$ita8_plus_mate8_std, alpha=0.01, B=3e3)

# Q42: L'utilizzo invalsi impatta su varianza miglioramento dei voti, in media, in italiano?
bootstrapReg(df$utilizzo_invalsi, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q43: L'utilizzo invalsi impatta su varianza miglioramento dei voti, in media, in matematica?
bootstrapReg(df$utilizzo_invalsi, df$improv_mate_rapp_std, alpha=0.01, B=2e3)




# Q44: il coinvolgimento prop dei genitori impatta su varianza voti, in media, in italiano?
hist(df$coinvolgimento_genitori_prop)
bootstrapReg(df$coinvolgimento_genitori_prop, df$ita8_std, alpha=0.01, B=2e3)

# Q45: * il coinvolgimento prop dei genitori impatta su varianza voti, in media, in matematica?
bootstrapReg(df$coinvolgimento_genitori_prop, df$mate8_std, alpha=0.01, B=2e3)

# Q46: * il coinvolgimento prop dei genitori impatta su varianza voti, in media, in italiano + matematica?
bootstrapReg(df$coinvolgimento_genitori_prop, df$ita8_plus_mate8_std, alpha=0.03, B=3e3)

# Q47: il coinvolgimento prop dei genitori impatta su varianza miglioramento dei voti, in media, in italiano?
bootstrapReg(df$coinvolgimento_genitori_prop, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q48: il coinvolgimento prop dei genitori impatta su varianza miglioramento dei voti, in media, in matematica?
bootstrapReg(df$coinvolgimento_genitori_prop, df$improv_mate_rapp_std, alpha=0.01, B=2e3)




# Q49: La presenza di strumenti impatta su varianza voti, in media, in italiano?
hist(df$strumenti)
bootstrapReg(df$strumenti, df$ita8_std, alpha=0.01, B=2e3)

# Q50: La presenza di strumenti impatta su varianza voti, in media, in italiano?
bootstrapReg(df$strumenti, df$mate8_std, alpha=0.01, B=2e3)

# Q51: La presenza di strumenti impatta su varianza voti, in media, in italiano + matematica?
bootstrapReg(df$strumenti, df$ita8_plus_mate8_std, alpha=0.05, B=3e3)

# Q52: La presenza di strumenti impatta su varianza miglioramento dei voti, in media, in italiano?
bootstrapReg(df$strumenti, df$improv_ita_rapp_std, alpha=0.01, B=2e3)

# Q53: La presenza di strumenti impatta su varianza miglioramento dei voti, in media, in matematica?
bootstrapReg(df$strumenti, df$improv_mate_rapp_std, alpha=0.01, B=2e3)




# Q54: Strumenti <> 1.1 ha impatto su varianza punteggi di italiano?
hist(df$strumenti_binary)
boxplot(df$ita8_std ~ df$strumenti_binary)
uniPermTest(df$ita8_std, df$strumenti_binary, B=1e4)

# Q55: Strumenti <> 1.1 impatta sui punteggi, in media, in matematica?
boxplot(df$mate8_mean ~ df$strumenti_binary)
uniPermTest(df$mate8_mean, df$strumenti_binary, B=1e4)

# Q56: Strumenti <> 1.1 ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$strumenti_binary, B=1e4)
plot(df$ita8_mean, df$mate8_mean, col=df$strumenti_binary+3, pch='*')

# Q57: * Strumenti <> 1.1 ha impatto sui miglioramenti in matematica?
boxplot(df$improv_mate_rapp_std ~ df$strumenti_binary)
uniPermTest(df$improv_mate_rapp_std, df$strumenti_binary, B=1e4)

# Q58: Strumenti <> 1.1 ha impatto sui miglioramenti in italiano?
boxplot(df$improv_ita_rapp_std ~ df$strumenti_binary)
uniPermTest(df$improv_ita_rapp_std, df$strumenti_binary, B=1e4)


# # # # # # # # # # Nord Italia # # # # # # # # # # #
#                                                   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Q1: studenti pi?? educati migliorano anche di pi??, in media, in italiano? 
hist(dfN$condotta_studenti)
bootstrapReg(dfN$condotta_studenti, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q2: studenti pi?? educati migliorano anche di pi??, in media, in matematica? 
bootstrapReg(dfN$condotta_studenti, dfN$improv_mate_rapp_mean, alpha=0.01, B=2e3)


# Q3: studenti pi?? educati hanno punteggi pi?? alti, in media, in italiano? 
hist(dfN$ita8_mean)
bootstrapReg(dfN$condotta_studenti, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q4: studenti pi?? educati hanno punteggi pi?? alti, in media, in matematica? 
hist(dfN$mate8_mean)
bootstrapReg(dfN$condotta_studenti, dfN$mate8_mean, alpha=0.01, B=2e3)


# Q5: l'attivit?? dei presidi impatta sui punteggi, in media, in italiano?
hist(dfN$attivita_preside)
bootstrapReg(dfN$attivita_preside, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q6: l'attivit?? dei presidi impatta sui punteggi, in media, in matematica?
bootstrapReg(dfN$attivita_preside, dfN$mate8_mean, alpha=0.01, B=2e3)


# Q7: l'attivit?? dei presidi impatta sui miglioramenti, in media, in italiano?
bootstrapReg(dfN$attivita_preside, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)


# Q8: l'attivit?? dei presidi impatta sui miglioramenti, in media, in matematica?
bootstrapReg(dfN$attivita_preside, dfN$improv_mate_rapp_mean, alpha=0.01, B=2e3)


# Q9: * La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di italiano?
hist(dfN$pressioni_genitori)
boxplot(dfN$ita8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)

# Q10: * La pressione dei genitori impatta sui punteggi, in media, in matematica?
hist(dfN$pressioni_genitori)
boxplot(dfN$mate8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)

# Q11: * La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano?
hist(dfN$pressioni_genitori)
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$pressioni_genitori, B=1e4)

# Q12: La pressione dei genitori, percepita dai presidi, ha impatto sui miglioramenti in matematica?
boxplot(dfN$improv_mate_rapp_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$improv_mate_rapp_mean, dfN$pressioni_genitori, B=1e4)

# Q13: La pressione dei genitori, percepita dai presidi, ha impatto sui miglioramenti in italiano?
boxplot(dfN$improv_ita_rapp_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$improv_ita_rapp_mean, dfN$pressioni_genitori, B=1e4)



# Q14: il coinvolgimento dei genitori impatta sui voti, in media, in italiano?
hist(dfN$coinvolgimento_genitori_eff)
bootstrapReg(dfN$coinvolgimento_genitori_eff, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q15: il coinvolgimento dei genitori impatta sui voti, in media, in italiano?
bootstrapReg(dfN$coinvolgimento_genitori_eff, dfN$mate8_mean, alpha=0.01, B=2e3)

# Q16: il coinvolgimento dei genitori impatta sui voti, in media, in italiano + matematica?
dfN$ita8_plus_mate8_mean = dfN$mate8_mean + dfN$ita8_mean
bootstrapReg(dfN$coinvolgimento_genitori_eff, dfN$ita8_plus_mate8_mean, alpha=0.01, B=3e3)

# Q17: il coinvolgimento dei genitori impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(dfN$coinvolgimento_genitori_eff, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q18: il coinvolgimento dei genitori impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(dfN$coinvolgimento_genitori_eff, dfN$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q19: L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di italiano?
hist(dfN$opinione_associazioni_genitori_binary)
boxplot(dfN$ita8_mean ~ dfN$opinione_associazioni_genitori_binary)
uniPermTest(dfN$ita8_mean, dfN$opinione_associazioni_genitori_binary, B=1e4)

# Q20: L'opinione sulle associazioni di genitori impatta sui punteggi, in media, in matematica?
hist(dfN$opinione_associazioni_genitori_binary)
boxplot(dfN$mate8_mean ~ dfN$opinione_associazioni_genitori_binary)
uniPermTest(dfN$mate8_mean, dfN$opinione_associazioni_genitori_binary, B=1e4)

# Q21: L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$opinione_associazioni_genitori_binary, B=1e4)
plot(dfN$ita8_mean, dfN$mate8_mean, col=dfN$opinione_associazioni_genitori_binary+3, pch='*')

# Q22: L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui miglioramenti in matematica?
boxplot(dfN$improv_mate_rapp_mean ~ dfN$opinione_associazioni_genitori_binary)
uniPermTest(dfN$improv_mate_rapp_mean, dfN$opinione_associazioni_genitori_binary, B=1e4)

# Q23: L'opinione delle associazioni di genitori, percepita dai presidi, ha impatto sui miglioramenti in italiano?
boxplot(dfN$improv_ita_rapp_mean ~ dfN$opinione_associazioni_genitori_binary)
uniPermTest(dfN$improv_ita_rapp_mean, dfN$opinione_associazioni_genitori_binary, B=1e4)





# Q24: Le infrastrutture impattano, in media, in italiano?
hist(dfN$infrastrutture)
bootstrapReg(dfN$infrastrutture, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q25: Le infrastrutture impattano, in media, in italiano?
bootstrapReg(dfN$infrastrutture, dfN$mate8_mean, alpha=0.01, B=2e3)

# Q26: Le infrastrutture impattano, in media, in italiano + matematica?
bootstrapReg(dfN$infrastrutture, dfN$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q27: Le infrastrutture impattano sul miglioramento dei voti, in media, in italiano?
bootstrapReg(dfN$infrastrutture, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q28: Le infrastrutture impattano sul miglioramento dei voti, in media, in matematica?
bootstrapReg(dfN$infrastrutture, dfN$improv_mate_rapp_mean, alpha=0.01, B=2e3)



# Q29: L'eta' preside impatta, in media, in italiano?
hist(dfN$eta_preside)
bootstrapReg(dfN$eta_preside, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q30: L'eta' preside impatta, in media, in italiano?
bootstrapReg(dfN$eta_preside, dfN$mate8_mean, alpha=0.01, B=2e3)

# Q31: L'eta' preside impatta, in media, in italiano + matematica?
bootstrapReg(dfN$eta_preside, dfN$ita8_plus_mate8_mean, alpha=0.01, B=3e3)

# Q32: L'eta' preside impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(dfN$eta_preside, dfN$ita_mean_improvement, alpha=0.01, B=2e3)

# Q33: L'eta' preside impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(dfN$eta_preside, dfN$mat_mean_improvement, alpha=0.01, B=2e3)




# Q34: Preside maschio ha impatto sui punteggi di italiano?
hist(dfN$preside_maschio)
boxplot(dfN$ita8_mean ~ dfN$preside_maschio)
uniPermTest(dfN$ita8_mean, dfN$preside_maschio, B=1e4)

# Q35: Preside maschio impatta sui punteggi, in media, in matematica?
boxplot(dfN$mate8_mean ~ dfN$preside_maschio)
uniPermTest(dfN$mate8_mean, dfN$preside_maschio, B=1e4)

# Q36: Preside maschio ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$preside_maschio, B=1e4)
plot(dfN$ita8_mean, dfN$mate8_mean, col=dfN$opinione_associazioni_genitori_binary+3, pch='*')

# Q37: Preside maschio ha impatto sui miglioramenti in matematica?
boxplot(dfN$improv_mate_rapp_mean ~ dfN$preside_maschio)
uniPermTest(dfN$improv_mate_rapp_mean, dfN$preside_maschio, B=1e4)

# Q38: Preside maschio ha impatto sui miglioramenti in italiano?
boxplot(dfN$improv_ita_rapp_mean ~ dfN$preside_maschio)
uniPermTest(dfN$improv_ita_rapp_mean, dfN$preside_maschio, B=1e4)



# Q39: L'utilizzo invalsi impatta, in media, in italiano?
hist(dfN$utilizzo_invalsi)
bootstrapReg(dfN$utilizzo_invalsi, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q40: * L'utilizzo invalsi impatta, in media, in matematica?
bootstrapReg(dfN$utilizzo_invalsi, dfN$mate8_mean, alpha=0.01, B=2e3)

# Q41: * L'utilizzo invalsi impatta, in media, in italiano + matematica?
bootstrapReg(dfN$utilizzo_invalsi, dfN$ita8_plus_mate8_mean, alpha=0.02, B=3e3)

# Q42: L'utilizzo invalsi impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(dfN$utilizzo_invalsi, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q43: L'utilizzo invalsi impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(dfN$utilizzo_invalsi, dfN$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q44: il coinvolgimento prop dei genitori impatta sui voti, in media, in italiano?
hist(dfN$coinvolgimento_genitori_prop)
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q45: il coinvolgimento prop dei genitori impatta sui voti, in media, in italiano?
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$mate8_mean, alpha=0.01, B=2e3)

# Q46: il coinvolgimento prop dei genitori impatta sui voti, in media, in italiano + matematica?
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q47: il coinvolgimento prop dei genitori impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q48: il coinvolgimento prop dei genitori impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$mat_mean_improvement, alpha=0.01, B=2e3)




# Q49: La presenza di strumenti impatta sui voti, in media, in italiano?
hist(dfN$strumenti)
bootstrapReg(dfN$strumenti, dfN$ita8_mean, alpha=0.01, B=2e3)

# Q50: La presenza di strumenti impatta sui voti, in media, in italiano?
bootstrapReg(dfN$strumenti, dfN$mate8_mean, alpha=0.01, B=2e3)

# Q51: La presenza di strumenti impatta sui voti, in media, in italiano + matematica?
bootstrapReg(dfN$strumenti, dfN$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# Q52: La presenza di strumenti impatta sul miglioramento dei voti, in media, in italiano?
bootstrapReg(dfN$strumenti, dfN$improv_ita_rapp_mean, alpha=0.01, B=2e3)

# Q53: La presenza di strumenti impatta sul miglioramento dei voti, in media, in matematica?
bootstrapReg(dfN$strumenti, dfN$improv_mate_rapp_mean, alpha=0.01, B=2e3)




# Q54: Strumenti <> 1.1 ha impatto sui punteggi di italiano?
hist(dfN$strumenti_binary)
boxplot(dfN$ita8_mean ~ dfN$strumenti_binary)
uniPermTest(dfN$ita8_mean, dfN$strumenti_binary, B=1e4)

# Q55: Strumenti <> 1.1 impatta sui punteggi, in media, in matematica?
boxplot(dfN$mate8_mean ~ dfN$strumenti_binary)
uniPermTest(dfN$mate8_mean, dfN$strumenti_binary, B=1e4)

# Q56: Strumenti <> 1.1 ha impatto sui punteggi di matematica e italiano?
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$strumenti_binary, B=1e4)
plot(dfN$ita8_mean, dfN$mate8_mean, col=dfN$strumenti_binary+3, pch='*')

# Q57: Strumenti <> 1.1 ha impatto sui miglioramenti in matematica?
boxplot(dfN$improv_mate_rapp_mean ~ dfN$strumenti_binary)
uniPermTest(dfN$improv_mate_rapp_mean, dfN$strumenti_binary, B=1e4)

# Q58: Strumenti <> 1.1 ha impatto sui miglioramenti in italiano?
boxplot(dfN$improv_ita_rapp_mean ~ dfN$strumenti_binary)
uniPermTest(dfN$improv_ita_rapp_mean, dfN$strumenti_binary, B=1e4)







# # # # # # Significant Differences Found # # # # #




# QA1 Data driven, improvement mate?
boxplot(dfp$improv_mate_rapp_mean ~ dfp$data_driven_principal)
tapply(dfp$improv_mate_rapp_mean, dfp$data_driven_principal, mean, na.rm=T)
uniPermTest(dfp$improv_mate_rapp_mean, dfp$data_driven_principal, B=1e4)

# QA2 Data driven, Nord o Sud?
boxplot(dfp$data_driven_principal ~ dfp$Nord)
tapply(dfp$data_driven_principal, dfp$Nord, mean, na.rm=T)
uniPermTest(dfp$data_driven_principal, dfp$Nord, B=1e4)

# QA3 Improvement Mate, Nord o Sud?
boxplot(dfp$improv_mate_rapp_mean ~ dfp$Nord)
tapply(dfp$improv_mate_rapp_mean, dfp$Nord, mean, na.rm=T)
uniPermTest(dfp$improv_mate_rapp_mean, dfp$Nord, B=1e4)

# QA4 Data Driven Principal al Nord 
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$data_driven_principal)
tapply(dfNp$improv_mate_rapp_mean, dfNp$data_driven_principal, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$data_driven_principal, B=1e4)

# QA4.2 Data driven principal e mate8
boxplot(dfNp$mate8_mean ~ dfNp$data_driven_principal)
tapply(dfNp$mate8_mean, dfNp$data_driven_principal, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$data_driven_principal, B=1e4)

# QA4.3 Data driven principal e ita
boxplot(dfNp$ita8_mean ~ dfNp$data_driven_principal)
tapply(dfNp$ita8_mean, dfNp$data_driven_principal, mean, na.rm=T)
uniPermTest(dfNp$ita8_mean, dfNp$data_driven_principal, B=1e4)




# QA5 Controllore, improvement mate?
boxplot(dfp$improv_mate_rapp_mean ~ dfp$controllore_principal)
tapply(dfp$improv_mate_rapp_mean, dfp$controllore_principal, mean, na.rm=T)
uniPermTest(dfp$improv_mate_rapp_mean, dfp$controllore_principal, B=1e4)

# QA6 Controllore, Nord o Sud?
boxplot(dfp$controllore_principal ~ dfp$Nord)
tapply(dfp$controllore_principal, dfp$Nord, mean, na.rm=T)
uniPermTest(dfp$controllore_principal, dfp$Nord, B=1e4)

# QA7 Improvement Mate, Nord o Sud?
boxplot(dfp$improv_mate_rapp_mean ~ dfp$Nord)
tapply(dfp$improv_mate_rapp_mean, dfp$Nord, mean, na.rm=T)
uniPermTest(dfp$improv_mate_rapp_mean, dfp$Nord, B=1e4)

# QA8 Controllori al Nord  e improvement medio
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$controllore_principal)
tapply(dfNp$improv_mate_rapp_mean, dfNp$controllore_principal, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$controllore_principal, B=1e4)

# QA9 Controllori al Nord  e mate
boxplot(dfNp$mate8_mean ~ dfNp$controllore_principal)
tapply(dfNp$mate8_mean, dfNp$controllore_principal, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$controllore_principal, B=1e4)



# * QA10 Innovatori della didattica e improvement medio
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$insegnanti_innovatori)
tapply(dfNp$improv_mate_rapp_mean, dfNp$insegnanti_innovatori, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$insegnanti_innovatori, B=1e4)

# QA11 Innovatori della didattica e mate8
boxplot(dfNp$mate8_mean ~ dfNp$insegnanti_innovatori)
tapply(dfNp$mate8_mean, dfNp$insegnanti_innovatori, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$insegnanti_innovatori, B=1e4)



# QA12 Relazioni tra studenti e improvement medio
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$relazioni_studenti)
tapply(dfNp$improv_mate_rapp_mean, dfNp$relazioni_studenti, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$relazioni_studenti, B=1e4)

# QA13 Relazioni tra studenti e mate8
boxplot(dfNp$mate8_mean ~ dfNp$relazioni_studenti)
tapply(dfNp$mate8_mean, dfNp$relazioni_studenti, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$relazioni_studenti, B=1e4)




# QA14 Scuole ricche e improvement medio
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$scuole_ricche)
tapply(dfNp$improv_mate_rapp_mean, dfNp$scuole_ricche, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$scuole_ricche, B=1e4)

# QA15 Scuole ricche e mate8
boxplot(dfNp$mate8_mean ~ dfNp$scuole_ricche)
tapply(dfNp$mate8_mean, dfNp$scuole_ricche, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$scuole_ricche, B=1e4)



# QA16 Partecipazione rappresentanti e improvement medio
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$partecipazione_rappre)
tapply(dfNp$improv_mate_rapp_mean, dfNp$partecipazione_rappre, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$partecipazione_rappre, B=1e4)

# QA17 Partecipazione rappresentanti e mate8
boxplot(dfNp$mate8_mean ~ dfNp$partecipazione_rappre)
tapply(dfNp$mate8_mean, dfNp$partecipazione_rappre, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$partecipazione_rappre, B=1e4)





# QA18 Honest Score e improvement medio
plot(dfNp$honest_score_presidi, dfNp$improv_mate_rapp_mean)
bootstrapReg(dfNp$honest_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=3e3)

boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$honest_score_presidi_class)
tapply(dfNp$improv_mate_rapp_mean, dfNp$honest_score_presidi_class, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$honest_score_presidi_class, B=1e4)

# QA19 Honest Score e mate8
boxplot(dfNp$mate8_mean ~ dfNp$honest_score_presidi_class)
tapply(dfNp$mate8_mean, dfNp$honest_score_presidi_class, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$honest_score_presidi_class, B=1e4)





# QA20 Data driven 2 e improvement 
boxplot(dfNp$improv_mate_rapp_mean ~ dfNp$data_driven_2)
tapply(dfNp$improv_mate_rapp_mean, dfNp$data_driven_2, mean, na.rm=T)
uniPermTest(dfNp$improv_mate_rapp_mean, dfNp$data_driven_2, B=1e4)

# QA21 Data driven 2 e mate8
boxplot(dfNp$mate8_mean ~ dfNp$data_driven_2)
tapply(dfNp$mate8_mean, dfNp$data_driven_2, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$data_driven_2, B=1e4)


# QA22 Data driven 2 e ita8
boxplot(dfNp$ita8_mean ~ dfNp$data_driven_2)
tapply(dfNp$ita8_mean, dfNp$data_driven_2, mean, na.rm=T)
uniPermTest(dfNp$ita8_mean, dfNp$data_driven_2, B=1e4)



# QA23 Integrazione con imprese e mate8
boxplot(dfNp$mate8_mean ~ dfNp$integrazione_imprese)
tapply(dfNp$mate8_mean, dfNp$integrazione_imprese, mean, na.rm=T)
uniPermTest(dfNp$mate8_mean, dfNp$integrazione_imprese, B=1e4)


# QA24 Integrazione con imprese e ita8
boxplot(dfNp$ita8_mean ~ dfNp$integrazione_imprese)
tapply(dfNp$ita8_mean, dfNp$integrazione_imprese, mean, na.rm=T)
uniPermTest(dfNp$ita8_mean, dfNp$integrazione_imprese, B=1e4)

# QA25 * and now, a very dumb ESCS con mate, mean
bootstrapReg(dfNp$ESCS_mean, dfNp$mate8_mean, alpha=0.01, B=3e3)

# QA26 and now, a very dumb ESCS con ita, mean
bootstrapReg(dfNp$ESCS_mean, dfNp$ita8_mean, alpha=0.01, B=3e3)

# QA27 and now, a very dumb ESCS con mate, std
bootstrapReg(dfNp$ESCS_mean, dfNp$mate8_std, alpha=0.01, B=3e3)

# QA28 and now, a very dumb ESCS con ita, mean
bootstrapReg(dfNp$ESCS_mean, dfNp$ita8_std, alpha=0.01, B=3e3)


#

# QB1: Debiased a and mate 8?
hist(dfNp$debiased_a_score_presidi)
bootstrapReg(dfNp$debiased_a_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB2: Debiased b and mate 8?
hist(dfNp$debiased_b_score_presidi)
bootstrapReg(dfNp$debiased_b_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB3: Debiased c and mate 8?
hist(dfNp$debiased_c_score_presidi)
bootstrapReg(dfNp$debiased_c_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB4: Debiased d and mate 8?
hist(dfNp$debiased_d_score_presidi)
bootstrapReg(dfNp$debiased_d_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB5: * Debiased e and mate 8?
hist(dfNp$debiased_e_score_presidi)
bootstrapReg(dfNp$debiased_e_score_presidi, dfNp$mate8_mean, alpha=0.05, B=4e3)

# QB6: Debiased f and mate 8?
hist(dfNp$debiased_f_score_presidi)
bootstrapReg(dfNp$debiased_f_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB7: Debiased g and mate 8?
hist(dfNp$debiased_g_score_presidi)
bootstrapReg(dfNp$debiased_g_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB8: Debiased h and mate 8?
hist(dfNp$debiased_h_score_presidi)
bootstrapReg(dfNp$debiased_h_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB9: Debiased i and mate 8?
hist(dfNp$debiased_i_score_presidi)
bootstrapReg(dfNp$debiased_i_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB10: Debiased j and mate 8?
hist(dfNp$debiased_j_score_presidi)
bootstrapReg(dfNp$debiased_j_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB11: Debiased k and mate 8?
hist(dfNp$debiased_k_score_presidi)
bootstrapReg(dfNp$debiased_k_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB12: Debiased l and mate 8?
hist(dfNp$debiased_l_score_presidi)
bootstrapReg(dfNp$debiased_l_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB13: Debiased m and mate 8?
hist(dfNp$debiased_m_score_presidi)
bootstrapReg(dfNp$debiased_m_score_presidi, dfNp$mate8_mean, alpha=0.01, B=2e3)

# QB14: * Debiased n and mate 8?
hist(dfNp$debiased_n_score_presidi)
plot(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, pch="*")
bootstrapReg(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, alpha=0.02, B=4e3)




# QB15: Debiased a and improvement mate 8?
bootstrapReg(dfNp$debiased_a_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB16: Debiased b and improvement mate 8?
bootstrapReg(dfNp$debiased_b_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB17: Debiased c and improvement mate 8?
bootstrapReg(dfNp$debiased_c_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB18: Debiased d and improvement mate 8?
bootstrapReg(dfNp$debiased_d_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB19: Debiased e and improvement mate 8?
bootstrapReg(dfNp$debiased_e_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=4e3)

# QB20: Debiased f and improvement mate 8?
bootstrapReg(dfNp$debiased_f_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB21: Debiased g and improvement mate 8?
bootstrapReg(dfNp$debiased_g_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB22: Debiased h and improvement mate 8?
bootstrapReg(dfNp$debiased_h_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB23: Debiased i and improvement mate 8?
bootstrapReg(dfNp$debiased_i_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB24: Debiased j and improvement mate 8?
bootstrapReg(dfNp$debiased_j_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB25: Debiased k and improvement mate 8?
bootstrapReg(dfNp$debiased_k_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB26: Debiased l and improvement mate 8?
bootstrapReg(dfNp$debiased_l_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB27: Debiased m and improvement mate 8?
bootstrapReg(dfNp$debiased_m_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB28: Debiased n and improvement mate 8?
bootstrapReg(dfNp$debiased_n_score_presidi, dfNp$improv_mate_rapp_mean, alpha=0.02, B=4e3)










# QB29: Debiased a and mate 8?
hist(dfp$debiased_a_score_presidi)
bootstrapReg(dfp$debiased_a_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB30: Debiased b and mate 8?
hist(dfp$debiased_b_score_presidi)
bootstrapReg(dfp$debiased_b_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB31: Debiased c and mate 8?
hist(dfp$debiased_c_score_presidi)
bootstrapReg(dfp$debiased_c_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB32: Debiased d and mate 8?
hist(dfp$debiased_d_score_presidi)
bootstrapReg(dfp$debiased_d_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB33: * Debiased e and mate 8?
hist(dfp$debiased_e_score_presidi)
bootstrapReg(dfp$debiased_e_score_presidi, dfp$mate8_mean, alpha=0.05, B=4e3)

# QB34: Debiased f and mate 8?
hist(dfp$debiased_f_score_presidi)
bootstrapReg(dfp$debiased_f_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB35: * Debiased g and mate 8?
hist(dfp$debiased_g_score_presidi)
bootstrapReg(dfp$debiased_g_score_presidi, dfp$mate8_mean, alpha=0.02, B=2e3)

# QB36: Debiased h and mate 8?
hist(dfp$debiased_h_score_presidi)
bootstrapReg(dfp$debiased_h_score_presidi, dfp$mate8_mean, alpha=0.10, B=1e4)

# QB37: Debiased i and mate 8?
hist(dfp$debiased_i_score_presidi)
bootstrapReg(dfp$debiased_i_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB38: Debiased j and mate 8?
hist(dfp$debiased_j_score_presidi)
bootstrapReg(dfp$debiased_j_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB39: Debiased k and mate 8?
hist(dfp$debiased_k_score_presidi)
bootstrapReg(dfp$debiased_k_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB40: Debiased l and mate 8?
hist(dfp$debiased_l_score_presidi)
bootstrapReg(dfp$debiased_l_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB41: * Debiased m and mate 8?
hist(dfp$debiased_m_score_presidi)
bootstrapReg(dfp$debiased_m_score_presidi, dfp$mate8_mean, alpha=0.01, B=2e3)

# QB42: Debiased n and mate 8?
hist(dfp$debiased_n_score_presidi)
bootstrapReg(dfp$debiased_n_score_presidi, dfp$mate8_mean, alpha=0.02, B=4e3)




# QB43: Debiased a and improv mate 8?
bootstrapReg(dfp$debiased_a_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB44: Debiased b and improv mate 8?
bootstrapReg(dfp$debiased_b_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB45: Debiased c and improv mate 8?
bootstrapReg(dfp$debiased_c_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB46: Debiased d and improv mate 8?
bootstrapReg(dfp$debiased_d_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB47: Debiased e and improv mate 8?
bootstrapReg(dfp$debiased_e_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=4e3)

# QB48: Debiased f and improv. mate 8?
bootstrapReg(dfp$debiased_f_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB49: Debiased g and improv. mate 8?
bootstrapReg(dfp$debiased_g_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB50: Debiased h and improv. mate 8?
bootstrapReg(dfp$debiased_h_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB51: Debiased i and improv. mate 8?
bootstrapReg(dfp$debiased_i_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB52: Debiased j and improv. mate 8?
bootstrapReg(dfp$debiased_j_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB53: Debiased k and improv. mate 8?
bootstrapReg(dfp$debiased_k_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB54: Debiased l and improv. mate 8?
bootstrapReg(dfp$debiased_l_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB55: Debiased m and improv. mate 8?
bootstrapReg(dfp$debiased_m_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.01, B=2e3)

# QB56: Debiased n and improv. mate 8?
bootstrapReg(dfp$debiased_n_score_presidi, dfp$improv_mate_rapp_mean, alpha=0.02, B=4e3)









# # # # # # # # # RISULTATI # # # # # # # # # 
#                                           #
# # # # # # # # # # # # # # # # # # # # # # # 


# Q5: * l'attivit?? dei presidi impatta sui punteggi, in media, in italiano? 
# Si', negativamente
hist(df$attivita_preside)
plot(df$attivita_preside, df$ita8_mean)
bootstrapReg(df$attivita_preside, df$ita8_mean, alpha=0.01, B=1e4)

# Q6: * l'attivit?? dei presidi impatta sui punteggi, in media, in matematica? 
# Si', negativamente
plot(df$attivita_preside, df$mate8_mean)
bootstrapReg(df$attivita_preside, df$mate8_mean, alpha=0.01, B=1e4)

# Q16: * il coinvolgimento dei genitori impatta sui voti, in media, in italiano + matematica? 
# Si', positivamente
df$ita8_plus_mate8_mean = df$mate8_mean + df$ita8_mean
plot(df$coinvolgimento_genitori_eff, df$ita8_plus_mate8_mean)
bootstrapReg(df$coinvolgimento_genitori_eff, df$ita8_plus_mate8_mean, alpha=0.05, B=1e4)

# Q19: * L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di italiano?
# Si', positivamente
hist(df$opinione_associazioni_genitori_binary)
boxplot(df$ita8_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$ita8_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# Q20: * L'opinione sulle associazioni di genitori impatta sui punteggi, in media, in matematica?
# Si', positivamente
boxplot(df$mate8_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$mate8_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# Q21: * L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano?
# Si', c'?? differenza
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$opinione_associazioni_genitori_binary, B=1e4)
plot(df$ita8_mean, df$mate8_mean, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# Q6: * l'attivit?? dei presidi impatta su varianza punteggi, in media, in matematica?
# Si', diminuisce la varianza
plot(df$attivita_preside, df$mate8_std, pch="*")
bootstrapReg(df$attivita_preside, df$mate8_std, alpha=0.01, B=1e4)


# Q11: * La pressione dei genitori, percepita dai presidi, ha impatto su varianza punteggi di matematica e italiano?
# Si', c'?? differenza...al 10%
boxplot(df$ita8_std ~ df$pressioni_genitori)
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$pressioni_genitori, B=1e4)
plot(df$ita8_std, df$mate8_std, col=df$pressioni_genitori + 1, pch="*")


# Q22: * L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto su varianza miglioramenti in matematica?
# S??, c'?? minor variabilit??
boxplot(df$improv_mate_rapp_std ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$improv_mate_rapp_std, df$opinione_associazioni_genitori_binary, B=1e4)

# Q34: * Preside maschio ha impatto su varianza punteggi di italiano?
# S??, c'?? maggiore variabilit??
hist(df$preside_maschio)
boxplot(df$ita8_std ~ df$preside_maschio)
uniPermTest(df$ita8_std, df$preside_maschio, B=1e4)

# Q35: * Preside maschio impatta su varianza punteggi in matematica?
# S??, c'?? maggiore variabilit??
boxplot(df$mate8_std ~ df$preside_maschio)
uniPermTest(df$mate8_std, df$preside_maschio, B=1e4)

# Q36: * Preside maschio ha impatto sui punteggi di matematica e italiano?
# S??, c'?? differenza al 0.7%
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$preside_maschio, B=1e4)
plot(df$ita8_std, df$mate8_std, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# Q45: * il coinvolgimento prop dei genitori impatta su varianza voti, in media, in matematica?
# S??, c'?? meno variabilit??
bootstrapReg(df$coinvolgimento_genitori_prop, df$mate8_std, alpha=0.01, B=1e4)


# Q57: * Strumenti <> 1.1 ha impatto su varianza miglioramenti in matematica?
# S??, diminuisce la varianza
boxplot(df$improv_mate_rapp_std ~ df$strumenti_binary)
uniPermTest(df$improv_mate_rapp_std, df$strumenti_binary, B=1e4)

# Q9: * La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di italiano? Al Nord
# S??, diminuisce la performance
hist(dfN$pressioni_genitori)
boxplot(dfN$ita8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)

# Q10: * La pressione dei genitori impatta sui punteggi, in media, in matematica? Al Nord
# S??, aumenta la performance
hist(dfN$pressioni_genitori)
boxplot(dfN$mate8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)

# Q11: * La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano? Al Nord
# S??, c'?? differenza al 0.2%
hist(dfN$pressioni_genitori)
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$pressioni_genitori, B=1e4)

# Q40: * L'utilizzo invalsi impatta, in media, in matematica? Al Nord
# S??, positivamente
bootstrapReg(dfN$utilizzo_invalsi, dfN$mate8_mean, alpha=0.01, B=1e4)

# Q41: * L'utilizzo invalsi impatta, in media, in italiano + matematica? Al Nord
# S??, positivamente
dfN$ita8_plus_mate8_mean = dfN$mate8_mean + dfN$ita8_mean
bootstrapReg(dfN$utilizzo_invalsi, dfN$ita8_plus_mate8_mean, alpha=0.02, B=1e4)

# Q1 * Attivita presidi e' diversa tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$attivita_preside ~ df$Nord)
uniPermTest(df$attivita_preside, df$Nord, B=1e4)

# Q3 * Coinvolgimento genitori prop e' diverso tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$coinvolgimento_genitori_prop ~ df$Nord)
uniPermTest(df$coinvolgimento_genitori_prop, df$Nord, B=1e4)

# Q4 * Infrastrutture e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$infrastrutture ~ df$Nord)
uniPermTest(df$infrastrutture, df$Nord, B=1e4)

# Q5 * Strumenti e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$strumenti ~ df$Nord)
uniPermTest(df$strumenti, df$Nord, B=1e4)


# Q6 * Numero classi e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$class_tot ~ df$Nord)
uniPermTest(df$class_tot, df$Nord, B=1e4)


# Q8 * Fattore correzione diverso tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$Fattore_correzione ~ df$Nord)
uniPermTest(df$Fattore_correzione, df$Nord, B=1e4)


# QB5: * Debiased e and mate 8? Al Nord
# S??, negativamente
hist(dfNp$debiased_e_score_presidi)
bootstrapReg(dfNp$debiased_e_score_presidi, dfNp$mate8_mean, alpha=0.05, B=1e4)

# QB14: * Debiased n and mate 8? Al Nord
# S??, positivamente
hist(dfNp$debiased_n_score_presidi)
plot(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, pch="*")
bootstrapReg(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, alpha=0.02, B=5e3)

# QB33: * Debiased e and mate 8?
# S??, negativamente
hist(dfp$debiased_e_score_presidi)
bootstrapReg(dfp$debiased_e_score_presidi, dfp$mate8_mean, alpha=0.05, B=5e3)

# QB35: * Debiased g and mate 8?
# S??, positivamente
hist(dfp$debiased_g_score_presidi)
bootstrapReg(dfp$debiased_g_score_presidi, dfp$mate8_mean, alpha=0.02, B=5e3)










# # # # # # # # # STRESS TESTING RISULTATI# # 
#                                           #
# # # # # # # # # # # # # # # # # # # # # # # 




# Q5: x l'attivit?? dei presidi impatta sui punteggi, in media, in italiano? 
# Si', negativamente
hist(df$attivita_preside)
plot(df$attivita_preside, df$ita8_mean)
bootstrapReg(df$attivita_preside, df$ita8_mean, alpha=0.01, B=1e4)

# Ma c'?? differenza attivit?? Nord-Sud?
# S??
boxplot(df$attivita_preside ~ df$Nord)
uniPermTest(df$attivita_preside, df$Nord, B=1e4)

# E c'?? differenza ita8 Nord-Sud?
# S??
boxplot(df$ita8_mean ~ df$Nord)
uniPermTest(df$ita8_mean, df$Nord, B=1e4)










# Q6: x l'attivit?? dei presidi impatta sui punteggi, in media, in matematica? 
# Si', negativamente
plot(df$attivita_preside, df$mate8_mean)
bootstrapReg(df$attivita_preside, df$mate8_mean, alpha=0.01, B=1e4)

# Ma c'?? differenza attivit?? Nord-Sud?
# S??
boxplot(df$attivita_preside ~ df$Nord)
uniPermTest(df$attivita_preside, df$Nord, B=1e4)

# E c'?? differenza mate8 Nord-Sud?
# S??
boxplot(df$mate8_mean ~ df$Nord)
uniPermTest(df$mate8_mean, df$Nord, B=1e4)

# Vale solo al nord?
# no
bootstrapReg(dfN$attivita_preside, dfN$mate8_mean, alpha=0.01, B=1e4)

# Vale solo al sud?
# no
bootstrapReg(dfS$attivita_preside, dfS$mate8_mean, alpha=0.01, B=1e4)

# Vale solo al centro?
# no
bootstrapReg(dfC$attivita_preside, dfC$mate8_mean, alpha=0.01, B=1e4)

# Vale solo al centro-nord?
# no
bootstrapReg(dfNC$attivita_preside, dfNC$mate8_mean, alpha=0.01, B=1e4)













# Q16: x il coinvolgimento dei genitori impatta sui voti, in media, in italiano + matematica? 
# Si', positivamente
plot(df$coinvolgimento_genitori_eff, df$ita8_plus_mate8_mean)
bootstrapReg(df$coinvolgimento_genitori_eff, df$ita8_plus_mate8_mean, alpha=0.05, B=1e4)

# Ma c'?? differenza attivit?? Nord-Sud?
# No
boxplot(df$coinvolgimento_genitori_eff ~ df$Nord)
uniPermTest(df$coinvolgimento_genitori_eff, df$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# No
bootstrapReg(dfN$coinvolgimento_genitori_eff, dfN$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# No
bootstrapReg(dfS$coinvolgimento_genitori_eff, dfS$ita8_plus_mate8_mean, alpha=0.05, B=3e3)

# S??, al 10%
bootstrapReg(dfC$coinvolgimento_genitori_eff, dfC$ita8_plus_mate8_mean, alpha=0.1, B=3e3)

# Al Centro Nord?
# No
bootstrapReg(dfNC$coinvolgimento_genitori_eff, dfNC$ita8_plus_mate8_mean, alpha=0.1, B=3e3)









# Q19: x L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di italiano?
# Si', positivamente
hist(df$opinione_associazioni_genitori_binary)
boxplot(df$ita8_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$ita8_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# * Ma c'?? differenza Nord-Sud?
boxplot(df$opinione_associazioni_genitori_binary ~ df$Nord)
tapply(df$opinione_associazioni_genitori_binary, df$Nord, mean, na.rm=T)
uniPermTest(df$opinione_associazioni_genitori_binary, df$Nord, B=3e3)

boxplot(df$ita8_mean ~ df$Nord)
tapply(df$ita8_mean, df$Nord, mean, na.rm=T)
uniPermTest(df$ita8_mean, df$Nord, B=3e3)

# Vale al Nord?
# No
uniPermTest(dfN$ita8_mean, dfN$opinione_associazioni_genitori_binary, B=3e3)

# Vale al Centro?
# No
uniPermTest(dfC$ita8_mean, dfC$opinione_associazioni_genitori_binary, B=3e3)

# Vale al Sud?
# No
uniPermTest(dfS$ita8_mean, dfS$opinione_associazioni_genitori_binary, B=3e3)

# Vale al Centro-Nord?
# No
uniPermTest(dfNC$ita8_mean, dfNC$opinione_associazioni_genitori_binary, B=3e3)








# Q20: x L'opinione sulle associazioni di genitori impatta sui punteggi, in media, in matematica?
# Si', positivamente
boxplot(df$mate8_mean ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$mate8_mean, df$opinione_associazioni_genitori_binary, B=1e4)

# * Ma c'?? differenza Nord-Sud?
boxplot(df$opinione_associazioni_genitori_binary ~ df$Nord)
tapply(df$opinione_associazioni_genitori_binary, df$Nord, mean, na.rm=T)
uniPermTest(df$opinione_associazioni_genitori_binary, df$Nord, B=3e3)

# Vale al Nord?
# No
uniPermTest(dfN$mate8_mean, dfN$opinione_associazioni_genitori_binary, B=3e3)

# Vale al Centro?
# No
uniPermTest(dfC$mate8_mean, dfC$opinione_associazioni_genitori_binary, B=3e3)

# Vale al Sud?
# No
uniPermTest(dfS$mate8_mean, dfS$opinione_associazioni_genitori_binary, B=3e3)

# Vale al Centro-Nord?
# No
uniPermTest(dfNC$mate8_mean, dfNC$opinione_associazioni_genitori_binary, B=3e3)








# Q21: x L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano?
# Si', c'?? differenza
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$opinione_associazioni_genitori_binary, B=1e4)
plot(df$ita8_mean, df$mate8_mean, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# Vedi sopra, i due punti








# Q6: x* l'attivit?? dei presidi impatta su varianza punteggi, in media, in matematica?
# Si', diminuisce la varianza
plot(df$attivita_preside, df$mate8_std, pch="*")
bootstrapReg(df$attivita_preside, df$mate8_std, alpha=0.01, B=1e4, xtitle="Coeff. of attivita_preside", ytitle="ECDF", title="")

# Ma c'?? differenza attivit?? Nord-Sud?
# S??
boxplot(df$attivita_preside ~ df$Nord)
uniPermTest(df$attivita_preside, df$Nord, B=1e4)

# Ma c'?? differenza varianza Nord-Sud?
# S??
boxplot(df$mate8_std ~ df$Nord)
uniPermTest(df$mate8_std, df$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# No
bootstrapReg(dfN$attivita_preside, dfN$mate8_std, alpha=0.05, B=3e3)

# * S?? 
bootstrapReg(dfS$attivita_preside, dfS$mate8_std, alpha=0.02, B=1e4)

# No
bootstrapReg(dfC$attivita_preside, dfC$mate8_std, alpha=0.1, B=3e3)

# Al Centro Nord?
# No
bootstrapReg(dfNC$attivita_preside, dfNC$mate8_std, alpha=0.1, B=3e3)









# Q11: x La pressione dei genitori, percepita dai presidi, ha impatto su varianza punteggi di matematica e italiano?
# Si', c'?? differenza...al 10%
boxplot(df$ita8_std ~ df$pressioni_genitori)
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$pressioni_genitori, B=1e4)
plot(df$ita8_std, df$mate8_std, col=df$pressioni_genitori + 1, pch="*")

# * Ma c'?? differenza Nord-Sud?
# S??, pi?? pressione al sud
boxplot(df$pressioni_genitori ~ df$Nord)
tapply(df$pressioni_genitori, df$Nord, mean, na.rm=T)
uniPermTest(df$pressioni_genitori, df$Nord, B=3e3)

# Vale al Nord?
# No
multiPermTest(data.frame(a=dfN$ita8_std, b=dfN$mate8_std), dfN$pressioni_genitori, B=3e3)

# Vale al Centro?
# No
multiPermTest(data.frame(a=dfC$ita8_std, b=dfC$mate8_std), dfC$pressioni_genitori, B=3e3)

# Vale al Sud?
# * Al 11%
multiPermTest(data.frame(a=dfS$ita8_std, b=dfS$mate8_std), dfS$pressioni_genitori, B=1e4)

# Vale al Centro-Nord?
# No
multiPermTest(data.frame(a=dfNC$ita8_std, b=dfNC$mate8_std), dfNC$pressioni_genitori, B=3e3)









# Q22: x L'opinione sulle associazioni di genitori, percepita dai presidi, ha impatto su varianza miglioramenti in matematica?
# S??, c'?? minor variabilit??
boxplot(df$improv_mate_rapp_std ~ df$opinione_associazioni_genitori_binary)
uniPermTest(df$improv_mate_rapp_std, df$opinione_associazioni_genitori_binary, B=1e4)











# Q34: * Preside maschio ha impatto su varianza punteggi di italiano?
# S??, c'?? maggiore variabilit??
hist(df$preside_maschio)
boxplot(df$ita8_std ~ df$preside_maschio)
uniPermTest(df$ita8_std, df$preside_maschio, B=1e4)

mannWhitneyTest(df$ita8_std, df$preside_maschio, B=1e4)

# Ma c'?? differenza Nord-Sud?
# No
boxplot(df$preside_maschio ~ df$Nord)
tapply(df$preside_maschio, df$Nord, mean, na.rm=T)
uniPermTest(df$preside_maschio, df$Nord, B=3e3)

# Vale al Nord?
# No
uniPermTest(dfN$ita8_std, dfN$preside_maschio, B=3e3)

# Vale al Centro?
# No
uniPermTest(dfC$ita8_std, dfC$preside_maschio, B=3e3)

# * Vale al Sud?
# S??
uniPermTest(dfS$ita8_std, dfS$preside_maschio, B=1e4)
mannWhitneyTest(dfS$ita8_std, dfS$preside_maschio, B=1e4)


# Vale al Centro-Nord?
# No
uniPermTest(dfNC$ita8_std, dfNC$preside_maschio, B=3e3)















# Q35: * Preside maschio impatta su varianza punteggi in matematica?
# S??, c'?? maggiore variabilit??
boxplot(df$mate8_std ~ df$preside_maschio)
uniPermTest(df$mate8_std, df$preside_maschio, B=1e4)

uniPermTest(df$ita8_std, df$preside_maschio, B=1e4)


# Ma c'?? differenza Nord-Sud?
# No
boxplot(df$preside_maschio ~ df$Nord)
tapply(df$preside_maschio, df$Nord, mean, na.rm=T)
uniPermTest(df$preside_maschio, df$Nord, B=3e3)

# Vale al Nord?
# * S??, al 2%
uniPermTest(dfN$mate8_std, dfN$preside_maschio, B=1e4)
mannWhitneyTest(dfN$mate8_std, dfN$preside_maschio, B=1e4)


# Vale al Centro?
# No
uniPermTest(dfC$mate8_std, dfC$preside_maschio, B=5e3)

# Vale al Sud?
# Ni
uniPermTest(dfS$mate8_std, dfS$preside_maschio, B=1e4)

# Vale al Centro-Nord?
# * S??, al 4%
uniPermTest(dfNC$mate8_std, dfNC$preside_maschio, B=1e4)











# Q36: * Preside maschio ha impatto sui punteggi di matematica?
# S??, c'?? differenza al 0.7%
multiPermTest(data.frame(a=df$ita8_std, b=df$mate8_std), df$preside_maschio, B=1e4)
plot(df$ita8_std, df$mate8_std, col=df$opinione_associazioni_genitori_binary+3, pch='*')

# * Ma c'?? differenza Nord-Sud?
# S??, pi?? pressione al sud
boxplot(df$preside_maschio ~ df$Nord)
tapply(df$preside_maschio, df$Nord, mean, na.rm=T)
uniPermTest(df$preside_maschio, df$Nord, B=3e3)

# Vale al Nord?
# * S??, al 6%
multiPermTest(data.frame(a=dfN$ita8_std, b=dfN$mate8_std), dfN$preside_maschio, B=1e4)

# Vale al Centro?
# No
multiPermTest(data.frame(a=dfC$ita8_std, b=dfC$mate8_std), dfC$preside_maschio, B=3e3)

# Vale al Sud?
# * S??, al 4%
multiPermTest(data.frame(a=dfS$ita8_std, b=dfS$mate8_std), dfS$preside_maschio, B=1e4)

# Vale al Centro-Nord?
# * S??, al 10%
multiPermTest(data.frame(a=dfNC$ita8_std, b=dfNC$mate8_std), dfNC$preside_maschio, B=3e3)













# Q45: xx* il coinvolgimento prop dei genitori impatta su varianza voti, in media, in matematica?
# S??, c'?? meno variabilit??
bootstrapReg(df$coinvolgimento_genitori_prop, df$mate8_std, alpha=0.01, B=1e4)
# anche in italiano?
# No
bootstrapReg(df$coinvolgimento_genitori_prop, df$ita8_std, alpha=0.01, B=1e4)


# Ma c'?? differenza attivit?? Nord-Sud?
# * S??
boxplot(df$coinvolgimento_genitori_prop ~ df$Nord)
uniPermTest(df$coinvolgimento_genitori_prop, df$Nord, B=1e4)

# Ma c'?? differenza varianza Nord-Sud?
# * S??
boxplot(df$mate8_std ~ df$Nord)
uniPermTest(df$mate8_std, df$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# No
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$mate8_std, alpha=0.05, B=3e3)

# No 
bootstrapReg(dfS$coinvolgimento_genitori_prop, dfS$mate8_std, alpha=0.02, B=3e3)

# * S??, al 5% ???
bootstrapReg(dfC$coinvolgimento_genitori_prop, dfC$mate8_std, alpha=0.05, B=1e4)

# Al Centro Nord?
# No
bootstrapReg(dfNC$coinvolgimento_genitori_prop, dfNC$mate8_std, alpha=0.05, B=3e3)

# VALE ANCHE PER ITALIANO?
# Vale anche al Nord, Sud e Centro?
# No
bootstrapReg(dfN$coinvolgimento_genitori_prop, dfN$ita8_std, alpha=0.05, B=3e3)

# No 
bootstrapReg(dfS$coinvolgimento_genitori_prop, dfS$ita8_std, alpha=0.1, B=3e3)

# * S??, al 15% ???
bootstrapReg(dfC$coinvolgimento_genitori_prop, dfC$ita8_std, alpha=0.15, B=1e4)

# Al Centro Nord?
# No
bootstrapReg(dfNC$coinvolgimento_genitori_prop, dfNC$mate8_std, alpha=0.05, B=3e3)













# Q57: x Strumenti <> 1.1 ha impatto su varianza miglioramenti in matematica?
# S??, diminuisce la varianza
boxplot(df$improv_mate_rapp_std ~ df$strumenti_binary)
uniPermTest(df$improv_mate_rapp_std, df$strumenti_binary, B=1e4)


dfN$strumenti_binary = as.integer(dfN$strumenti > 1.1)
dfC$strumenti_binary = as.integer(dfC$strumenti > 1.1)
dfS$strumenti_binary = as.integer(dfS$strumenti > 1.1)
dfNC$strumenti_binary = as.integer(dfNC$strumenti > 1.1)



# Ma c'?? differenza Nord-Sud?
# * S??
boxplot(df$strumenti ~ df$Nord)
tapply(df$strumenti, df$Nord, mean, na.rm=T)
uniPermTest(df$strumenti, df$Nord, B=3e3)

# * S??
boxplot(df$improv_mate_rapp_std ~ df$Nord)
tapply(df$improv_mate_rapp_std, df$Nord, mean, na.rm=T)
uniPermTest(df$improv_mate_rapp_std, df$Nord, B=3e3)

# Vale al Nord?
# No
uniPermTest(dfN$improv_mate_rapp_std, dfN$strumenti_binary, B=1e4)

# Vale al Centro?
# No
uniPermTest(dfC$improv_mate_rapp_std, dfC$strumenti_binary, B=5e3)

# Vale al Sud?
# No
uniPermTest(dfS$improv_mate_rapp_std, dfS$strumenti_binary, B=1e4)

# Vale al Centro-Nord?
# No
uniPermTest(dfNC$improv_mate_rapp_std, dfNC$strumenti_binary, B=1e4)

















# Q9: * La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di italiano? Al Nord
# S??, diminuisce la performance
hist(dfN$pressioni_genitori)
boxplot(dfN$ita8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)
mannWhitneyTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)

# Anche in tutta italia?
boxplot(df$ita8_mean ~ df$pressioni_genitori)
uniPermTest(df$ita8_mean, df$pressioni_genitori, B=1e4)


# Ma c'?? differenza Nord-Sud?
# * S??
boxplot(df$pressioni_genitori ~ df$Nord)
tapply(df$pressioni_genitori, df$Nord, mean, na.rm=T)
uniPermTest(df$pressioni_genitori, df$Nord, B=3e3)

# * S??
boxplot(df$ita8_mean ~ df$Nord)
tapply(df$ita8_mean, df$Nord, mean, na.rm=T)
uniPermTest(df$ita8_mean, df$Nord, B=3e3)

# Vale al Nord?
# * S??, al 1%
boxplot(dfN$ita8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)
mannWhitneyTest(dfN$ita8_mean, dfN$pressioni_genitori, B=1e4)

# Vale al Centro?
# No
uniPermTest(dfC$ita8_mean, dfC$pressioni_genitori, B=5e3)
mannWhitneyTest(dfC$ita8_mean, dfC$pressioni_genitori, B=5e3)

# Vale al Sud?
# No
uniPermTest(dfS$ita8_mean, dfS$pressioni_genitori, B=1e4)
mannWhitneyTest(dfS$ita8_mean, dfS$pressioni_genitori, B=1e4)

# Vale al Centro-Nord?
# S??, al 10%
uniPermTest(dfNC$ita8_mean, dfNC$pressioni_genitori, B=1e4)








# Q10: * La pressione dei genitori impatta sui punteggi, in media, in matematica? Al Nord
# S??, aumenta la performance
hist(dfN$pressioni_genitori)
boxplot(dfN$mate8_mean ~ dfN$pressioni_genitori)
mannWhitneyTest(dfN$mate8_mean, dfN$pressioni_genitori, B=1e4)


# Ma c'?? differenza Nord-Sud?
# * S??
boxplot(df$pressioni_genitori ~ df$Nord)
tapply(df$pressioni_genitori, df$Nord, mean, na.rm=T)
uniPermTest(df$pressioni_genitori, df$Nord, B=3e3)

# * S??
boxplot(df$mate8_mean ~ df$Nord)
tapply(df$mate8_mean, df$Nord, mean, na.rm=T)
uniPermTest(df$mate8_mean, df$Nord, B=3e3)

# Vale al Nord?
# * S??, al 3%
boxplot(dfN$mate8_mean ~ dfN$pressioni_genitori)
uniPermTest(dfN$mate8_mean, dfN$pressioni_genitori, B=1e4)
mannWhitneyTest(dfN$mate8_mean, dfN$pressioni_genitori, B=1e4)


# Vale al Centro?
# No
uniPermTest(dfC$mate8_mean, dfC$pressioni_genitori, B=1e4)

# Vale al Sud?
# No 
boxplot(dfS$mate8_mean ~ dfS$pressioni_genitori)
uniPermTest(dfS$mate8_mean, dfS$pressioni_genitori, B=1e4)
mannWhitneyTest(dfS$mate8_mean, dfS$pressioni_genitori, B=1e4)

# Vale al Centro-Nord?
# No
uniPermTest(dfNC$mate8_mean, dfNC$pressioni_genitori, B=1e4)










# Q11: * La pressione dei genitori, percepita dai presidi, ha impatto sui punteggi di matematica e italiano? Al Nord
# S??, c'?? differenza al 0.2%
hist(dfN$pressioni_genitori)
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$pressioni_genitori, B=1e4)

# * Ma c'?? differenza Nord-Sud?
# S??, pi?? pressione al sud
boxplot(df$pressioni_genitori ~ df$Nord)
tapply(df$pressioni_genitori, df$Nord, mean, na.rm=T)
uniPermTest(df$pressioni_genitori, df$Nord, B=3e3)

# S??
multiPermTest(data.frame(a=df$ita8_mean, b=df$mate8_mean), df$Nord, B=1e4)


# Vale al Nord?
# * S??
multiPermTest(data.frame(a=dfN$ita8_mean, b=dfN$mate8_mean), dfN$pressioni_genitori, B=1e4)

# Vale al Centro?
# No
multiPermTest(data.frame(a=dfC$ita8_mean, b=dfC$mate8_mean), dfC$pressioni_genitori, B=3e3)

# Vale al Sud?
# * N?? al 10%
multiPermTest(data.frame(a=dfS$ita8_mean, b=dfS$mate8_mean), dfS$pressioni_genitori, B=1e4)

# Vale al Centro-Nord?
# No
multiPermTest(data.frame(a=dfNC$ita8_mean, b=dfNC$mate8_mean), dfNC$pressioni_genitori, B=3e3)














# Q40: * L'utilizzo invalsi impatta, in media, in matematica? Al Nord
# S??, positivamente
bootstrapReg(dfN$utilizzo_invalsi, dfN$mate8_mean, alpha=0.01, B=1e4)

# And in the whole Italy?
bootstrapReg(df$utilizzo_invalsi, df$mate8_mean, alpha=0.01, B=1e4)


# C'?? differenza attivit?? Nord-Sud?
# No
boxplot(df$utilizzo_invalsi ~ df$Nord)
uniPermTest(df$utilizzo_invalsi, df$Nord, B=1e4)

# C'?? differenza varianza Nord-Sud?
# * S??
boxplot(df$mate8_mean ~ df$Nord)
uniPermTest(df$mate8_mean, df$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# * S??
bootstrapReg(dfN$utilizzo_invalsi, dfN$mate8_mean, alpha=0.01, B=3e3)

# No 
bootstrapReg(dfS$utilizzo_invalsi, dfS$mate8_mean, alpha=0.02, B=3e3)

# * S??, al 5%
bootstrapReg(dfC$utilizzo_invalsi, dfC$mate8_mean, alpha=0.05, B=1e4)

# Al Centro Nord?
# ** S??
bootstrapReg(dfNC$utilizzo_invalsi, dfNC$mate8_mean, alpha=0.01, B=3e3)













# Q41: x L'utilizzo invalsi impatta, in media, in italiano? Al Nord
# S??, positivamente
bootstrapReg(dfN$utilizzo_invalsi, dfN$ita8_mean, alpha=0.02, B=1e4)

# C'?? differenza attivit?? Nord-Sud?
# No
boxplot(df$utilizzo_invalsi ~ df$Nord)
uniPermTest(df$utilizzo_invalsi, df$Nord, B=1e4)

# C'?? differenza varianza Nord-Sud?
# * S??
boxplot(df$ita8_mean ~ df$Nord)
uniPermTest(df$ita8_mean, df$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# No
bootstrapReg(dfN$utilizzo_invalsi, dfN$ita8_mean, alpha=0.02, B=1e4)

# No 
bootstrapReg(dfS$utilizzo_invalsi, dfS$ita8_mean, alpha=0.02, B=3e3)

# No
bootstrapReg(dfC$utilizzo_invalsi, dfC$ita8_mean, alpha=0.03, B=1e4)

# Al Centro Nord?
# No
bootstrapReg(dfNC$utilizzo_invalsi, dfNC$ita8_mean, alpha=0.02, B=3e3)










# Q1 * Attivita presidi e' diversa tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$attivita_preside ~ df$Nord)
uniPermTest(df$attivita_preside, df$Nord, B=1e4)

# Q3 * Coinvolgimento genitori prop e' diverso tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$coinvolgimento_genitori_prop ~ df$Nord)
uniPermTest(df$coinvolgimento_genitori_prop, df$Nord, B=1e4)

# Q4 * Infrastrutture e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$infrastrutture ~ df$Nord)
uniPermTest(df$infrastrutture, df$Nord, B=1e4)

# Q5 * Strumenti e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$strumenti ~ df$Nord)
uniPermTest(df$strumenti, df$Nord, B=1e4)


# Q6 * Numero classi e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$class_tot ~ df$Nord)
uniPermTest(df$class_tot, df$Nord, B=1e4)


# Q8 * Fattore correzione diverso tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$Fattore_correzione ~ df$Nord)
uniPermTest(df$Fattore_correzione, df$Nord, B=1e4)



# # # # # # # # # # # #



# QB5: * Debiased e and mate 8? Al Nord
# S??, negativamente
hist(dfNp$debiased_e_score_presidi)
bootstrapReg(dfNp$debiased_e_score_presidi, dfNp$mate8_mean, alpha=0.05, B=1e4)

# anche italiano?
bootstrapReg(dfNp$debiased_e_score_presidi, dfNp$ita8_mean, alpha=0.05, B=1e4)


# C'?? differenza debiased e Nord-Sud?
# * S??, pi?? basso al nord
boxplot(dfp$debiased_e_score_presidi ~ dfp$Nord)
uniPermTest(dfp$debiased_e_score_presidi, df$Nord, B=1e4)

# C'?? differenza mate8 Nord-Sud?
# * S??
boxplot(dfp$mate8_mean ~ dfp$Nord)
uniPermTest(df$mate8_mean, dfp$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# * S??, al 5%
bootstrapReg(dfNp$debiased_e_score_presidi, dfNp$mate8_mean, alpha=0.05, B=1e4)

# No 
bootstrapReg(dfSp$debiased_e_score_presidi, dfSp$mate8_mean, alpha=0.05, B=3e3)

# No
bootstrapReg(dfCp$debiased_e_score_presidi, dfCp$mate8_mean, alpha=0.03, B=5e3)

# Al Centro Nord?
# * S??, al 5%
bootstrapReg(dfNCp$debiased_e_score_presidi, dfNCp$mate8_mean, alpha=0.05, B=3e3)

# In tutta Italia?
# * S??
bootstrapReg(dfp$debiased_e_score_presidi, dfp$mate8_mean, alpha=0.01, B=3e3)












# QB14: * Debiased n and mate 8? Al Nord
# S??, positivamente
hist(dfNp$debiased_n_score_presidi)
plot(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, pch="*")
bootstrapReg(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, alpha=0.02, B=1e4)

# C'?? differenza debiased e Nord-Sud?
# No
boxplot(dfp$debiased_n_score_presidi ~ dfp$Nord)
uniPermTest(dfp$debiased_n_score_presidi, df$Nord, B=1e4)

# C'?? differenza mate8 Nord-Sud?
# * S??
boxplot(dfp$mate8_mean ~ dfp$Nord)
uniPermTest(df$mate8_mean, dfp$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# * S??, al 2%
bootstrapReg(dfNp$debiased_n_score_presidi, dfNp$mate8_mean, alpha=0.02, B=1e4)

# Anche italiano?
# No
bootstrapReg(dfNp$debiased_n_score_presidi, dfNp$ita8_mean, alpha=0.02, B=1e4)


# * S??, al Sud, ma negativo 
bootstrapReg(dfSp$debiased_n_score_presidi, dfSp$mate8_mean, alpha=0.05, B=1e4)

# No
bootstrapReg(dfCp$debiased_n_score_presidi, dfCp$mate8_mean, alpha=0.03, B=3e3)

# Al Centro Nord?
# * S??, al 4%
bootstrapReg(dfNCp$debiased_n_score_presidi, dfNCp$mate8_mean, alpha=0.04, B=5e3)

# In tutta Italia?
# No
bootstrapReg(dfp$debiased_n_score_presidi, dfp$mate8_mean, alpha=0.01, B=3e3)

# In italiano?
bootstrapReg(dfp$debiased_n_score_presidi, dfp$ita8_mean, alpha=0.01, B=3e3)















# QB33: x Debiased e and mate 8?
# S??, negativamente
hist(dfp$debiased_e_score_presidi)
bootstrapReg(dfp$debiased_e_score_presidi, dfp$mate8_mean, alpha=0.05, B=5e3)

# C'?? differenza debiased e Nord-Sud?
# * S??
boxplot(dfp$debiased_e_score_presidi ~ dfp$Nord)
uniPermTest(dfp$debiased_e_score_presidi, df$Nord, B=1e4)

# C'?? differenza mate8 Nord-Sud?
# * S??
boxplot(dfp$mate8_mean ~ dfp$Nord)
uniPermTest(df$mate8_mean, dfp$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# Fatto poco sopra







# QB35: x Debiased g and mate 8?
# S??, positivamente
hist(dfp$debiased_g_score_presidi)
bootstrapReg(dfp$debiased_g_score_presidi, dfp$mate8_mean, alpha=0.02, B=5e3)

# C'?? differenza debiased g Nord-Sud?
# * S??
boxplot(dfp$debiased_g_score_presidi ~ dfp$Nord)
uniPermTest(dfp$debiased_g_score_presidi, df$Nord, B=1e4)

# C'?? differenza mate8 Nord-Sud?
# * S??
boxplot(dfp$mate8_mean ~ dfp$Nord)
uniPermTest(df$mate8_mean, dfp$Nord, B=1e4)


# Vale anche al Nord, Sud e Centro?
# No
bootstrapReg(dfNp$debiased_g_score_presidi, dfNp$mate8_mean, alpha=0.02, B=1e4)

# Anche italiano, in tutta italia?
# No
# * S?? in ita + mat, al 5%
bootstrapReg(dfp$debiased_g_score_presidi, (dfp$mate8_mean + dfp$ita8_mean), alpha=0.05, B=1e4)


bootstrapReg(dfp$debiased_g_score_presidi, (dfp$mate8_mean), alpha=0.05, B=1e4)


# No al sud
bootstrapReg(dfSp$debiased_g_score_presidi, dfSp$mate8_mean, alpha=0.05, B=3e3)

# No al centro
bootstrapReg(dfCp$debiased_g_score_presidi, dfCp$mate8_mean, alpha=0.1, B=3e3)


# Al Centro Nord?
# No
bootstrapReg(dfNCp$debiased_g_score_presidi, dfNCp$mate8_mean, alpha=0.04, B=5e3)

