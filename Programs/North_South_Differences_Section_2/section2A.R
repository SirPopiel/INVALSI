#' Code for Section 2A (Geographical differences in factors, indices and outcomes)
#' Lines 11-13: general imports
#' Lines 16-105: Differences in factors and indices
#' Lines 106-end: Differences in outcomes
#'
#' @author F. Capello 


rm(list=ls())
source("./utils.R")
source("./prepare_data.R")


# # # # # # # # # DIFFERENZE NORD-SUD # # # # # # # # # # # # #
#                                                             #
#                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Q1 * Attivita presidi e' diversa tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$attivita_preside ~ df$Nord)
mannWhitneyTest(df$attivita_preside, df$Nord, B=1e4)

# Q2 * Coinvolgimento genitori prop e' diverso tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$coinvolgimento_genitori_prop ~ df$Nord)
mannWhitneyTest(df$coinvolgimento_genitori_prop, df$Nord, B=1e4)

# Q3 * Infrastrutture e' diverso tra Nord e Centro-Sud?
# S??, al 0.5%
boxplot(df$infrastrutture ~ df$Nord)
mannWhitneyTest(df$infrastrutture, df$Nord, B=1e4)

# Q4 * Strumenti e' diverso tra Nord e Centro-Sud?
# S??, al 0.2%
boxplot(df$strumenti ~ df$Nord)
mannWhitneyTest(df$strumenti, df$Nord, B=1e4)


# Q5 * Numero classi e' diverso tra Nord e Centro-Sud?
# No
boxplot(df$class_tot ~ df$Nord)
mannWhitneyTest(df$class_tot, df$Nord, B=1e4)


# Q6 * Fattore correzione diverso tra Nord e Centro-Sud?
# S??, al 0%
boxplot(df$Fattore_correzione ~ df$Nord)
mannWhitneyTest(df$Fattore_correzione, df$Nord, B=1e4)

# Q7 * Il bias sulla domanda D11 ?? diverso tra Nord e Sud?
boxplot(dfp$bias_score_presidi ~ dfp$Nord)
mannWhitneyTest(dfp$bias_score_presidi, dfp$Nord, B=1e4)

# Q8 * Utilizzo Invalsi?
# S??, al 9%
boxplot(df$utilizzo_invalsi ~ df$Nord)
tapply(df$utilizzo_invalsi, df$Nord, mean, na.rm=T)
mannWhitneyTest(df$utilizzo_invalsi, df$Nord, B=1e4)


# Q9 x condotta_studenti?
# No
boxplot(df$condotta_studenti ~ df$Nord)
mannWhitneyTest(df$condotta_studenti, df$Nord, B=1e4)


# Q10 x coinvolgimento_genitori_eff?
# No
boxplot(df$coinvolgimento_genitori_eff ~ df$Nord)
mannWhitneyTest(df$coinvolgimento_genitori_eff, df$Nord, B=1e4)


# Q11 x eta_preside?
# No
boxplot(df$eta_preside ~ df$Nord)
mannWhitneyTest(df$eta_preside, df$Nord, B=1e4)


# Q12 * pressioni_genitori?
# Sì
boxplot(df$pressioni_genitori ~ df$Nord)
mannWhitneyTest(df$pressioni_genitori, df$Nord, B=1e4)


# Q13 * opinione_associazioni_genitori_binary?
# Sì
boxplot(df$opinione_associazioni_genitori_binary ~ df$Nord)
mannWhitneyTest(df$opinione_associazioni_genitori_binary, df$Nord, B=1e4)


# Q14 x preside_maschio?
# No
boxplot(df$preside_maschio ~ df$Nord)
mannWhitneyTest(df$preside_maschio, df$Nord, B=1e4)


# Q15 * Strumenti e' diverso tra Nord e Centro-Sud?
# S??, al 4%
boxplot(df$strumenti_binary ~ df$Nord)
mannWhitneyTest(df$strumenti_binary, df$Nord, B=1e4)



# # # # # # Differences in Outcomes # # # # 

# Q16 * mate8_mean?
boxplot(df$mate8_mean ~ df$Nord)
mannWhitneyTest(df$mate8_mean, df$Nord, B=1e4)

# Q17 * ita8_mean?
boxplot(df$ita8_mean ~ df$Nord)
mannWhitneyTest(df$ita8_mean, df$Nord, B=1e4)

# Q18 * mate8_std?
boxplot(df$mate8_std ~ df$Nord)
mannWhitneyTest(df$mate8_std, df$Nord, B=1e4)

# Q19 * ita8_std?
boxplot(df$ita8_std ~ df$Nord)
mannWhitneyTest(df$ita8_std, df$Nord, B=1e4)

# Q20 * improv_mate_rapp_mean?
boxplot(df$improv_mate_rapp_mean ~ df$Nord)
mannWhitneyTest(df$improv_mate_rapp_mean, df$Nord, B=1e4)

# Q21 * improv_ita_rapp_mean?
boxplot(df$improv_ita_rapp_mean ~ df$Nord)
tapply(df$improv_ita_rapp_mean, df$Nord, mean, na.rm=T)
mannWhitneyTest(df$improv_ita_rapp_mean, df$Nord, B=1e4)

# Q22 x improv_mate_rapp_std?
boxplot(df$improv_mate_rapp_std ~ df$Nord)
tapply(df$improv_mate_rapp_std, df$Nord, mean, na.rm=T)
mannWhitneyTest(df$improv_mate_rapp_std, df$Nord, B=1e4)


