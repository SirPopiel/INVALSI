#' Code for Data Preparation for Sections 3 and 2A
#'
#' @author F. Capello 



# Import data
grades <- read.csv('../Data/group_invalsi.csv', header=T)
df = read.csv("../Data/gppp.csv", header=T)

dfN = df[df$Nord == 1,]

df$strumenti_binary = as.integer(df$strumenti > 1.1)
dfN$opinione_associazioni_genitori_binary = as.integer(dfN$opinione_associazioni_genitori > 0)
dfN$strumenti_binary = as.integer(dfN$strumenti > 1.1)

# Data with single responses
dfp = merge(x = df, y = grades, by = "cod_scu_anonimo", all.x = TRUE, suffixes = c("",".y"))
dfNp = merge(x = dfN, y = grades, by = "cod_scu_anonimo", all.x = TRUE, suffixes = c("",".y"))

# Innovatori
hist(dfNp$D14_i)
median(dfNp$D14_i)
dfNp$insegnanti_innovatori = as.integer(dfNp$D14_i >= 7)# + as.integer(dfNp$D11_f == "Spesso")

# Relazioni studenti
hist(dfNp$D14_a)
median(dfNp$D14_a)
dfNp$relazioni_studenti = as.integer(dfNp$D14_a >= 8)

# Scuole ricche
hist(dfNp$D14_k)
median(dfNp$D14_k)
dfNp$scuole_ricche = as.integer(dfNp$D14_k > 7)

# Partecipazione Rappresentanti
hist(dfNp$D10_j)
median(dfNp$D10_j)
dfNp$partecipazione_rappre = as.integer(dfNp$D10_j >= 4)


# Honest Score Presidi

dfNp$honest_score_presidi = (
  as.integer(dfNp$D11_a == "Sempre o q") + as.integer(dfNp$D11_f == "Sempre o q") +
    as.integer(dfNp$D11_b == "Sempre o q") + as.integer(dfNp$D11_g == "Sempre o q") +
    as.integer(dfNp$D11_c == "Sempre o q") + as.integer(dfNp$D11_h == "Sempre o q") +
    as.integer(dfNp$D11_d == "Sempre o q") + as.integer(dfNp$D11_i == "Sempre o q") +
    as.integer(dfNp$D11_e == "Sempre o q") + as.integer(dfNp$D11_j == "Sempre o q") +
    as.integer(dfNp$D11_k == "Sempre o q") + as.integer(dfNp$D11_m == "Sempre o q") +
    as.integer(dfNp$D11_l == "Sempre o q") + as.integer(dfNp$D11_n == "Sempre o q")
) / 14.0

hist(dfNp$honest_score_presidi)
dfNp$honest_score_presidi_class = as.integer(dfNp$honest_score_presidi > 0.9)


# Data driven 2
dfNp$data_driven_2 = as.integer(dfNp$D12_b == "Molto cont") + as.integer(dfNp$D12_b == "Contrario")
table(dfNp$data_driven_2)


# Integrazione con Imprese
hist(dfNp$D14_l)
median(dfNp$D14_l)
dfNp$integrazione_imprese = as.integer(dfNp$D14_l >= 3)


# # # # # # Debiasing indici presidi : NORD

dfNp$bias_score_presidi = (
  1*as.integer(dfNp$D11_a == "Mai o quas") + 2*as.integer(dfNp$D11_a == "Qualche vo") + 3*as.integer(dfNp$D11_a == "Spesso") + 4*as.integer(dfNp$D11_a == "Sempre o q") +
    1*as.integer(dfNp$D11_b == "Mai o quas") + 2*as.integer(dfNp$D11_b == "Qualche vo") + 3*as.integer(dfNp$D11_b == "Spesso") + 4*as.integer(dfNp$D11_b == "Sempre o q") +
    1*as.integer(dfNp$D11_c == "Mai o quas") + 2*as.integer(dfNp$D11_c == "Qualche vo") + 3*as.integer(dfNp$D11_c == "Spesso") + 4*as.integer(dfNp$D11_c == "Sempre o q") +
    1*as.integer(dfNp$D11_d == "Mai o quas") + 2*as.integer(dfNp$D11_d == "Qualche vo") + 3*as.integer(dfNp$D11_d == "Spesso") + 4*as.integer(dfNp$D11_d == "Sempre o q") +
    1*as.integer(dfNp$D11_e == "Mai o quas") + 2*as.integer(dfNp$D11_e == "Qualche vo") + 3*as.integer(dfNp$D11_e == "Spesso") + 4*as.integer(dfNp$D11_e == "Sempre o q") +
    1*as.integer(dfNp$D11_f == "Mai o quas") + 2*as.integer(dfNp$D11_f == "Qualche vo") + 3*as.integer(dfNp$D11_f == "Spesso") + 4*as.integer(dfNp$D11_f == "Sempre o q") +
    1*as.integer(dfNp$D11_g == "Mai o quas") + 2*as.integer(dfNp$D11_g == "Qualche vo") + 3*as.integer(dfNp$D11_g == "Spesso") + 4*as.integer(dfNp$D11_g == "Sempre o q") +
    1*as.integer(dfNp$D11_h == "Mai o quas") + 2*as.integer(dfNp$D11_h == "Qualche vo") + 3*as.integer(dfNp$D11_h == "Spesso") + 4*as.integer(dfNp$D11_h == "Sempre o q") +
    1*as.integer(dfNp$D11_i == "Mai o quas") + 2*as.integer(dfNp$D11_i == "Qualche vo") + 3*as.integer(dfNp$D11_i == "Spesso") + 4*as.integer(dfNp$D11_i == "Sempre o q") +
    1*as.integer(dfNp$D11_j == "Mai o quas") + 2*as.integer(dfNp$D11_j == "Qualche vo") + 3*as.integer(dfNp$D11_j == "Spesso") + 4*as.integer(dfNp$D11_j == "Sempre o q") +
    1*as.integer(dfNp$D11_k == "Mai o quas") + 2*as.integer(dfNp$D11_k == "Qualche vo") + 3*as.integer(dfNp$D11_k == "Spesso") + 4*as.integer(dfNp$D11_k == "Sempre o q") +
    1*as.integer(dfNp$D11_l == "Mai o quas") + 2*as.integer(dfNp$D11_l == "Qualche vo") + 3*as.integer(dfNp$D11_l == "Spesso") + 4*as.integer(dfNp$D11_l == "Sempre o q") +
    1*as.integer(dfNp$D11_m == "Mai o quas") + 2*as.integer(dfNp$D11_m == "Qualche vo") + 3*as.integer(dfNp$D11_m == "Spesso") + 4*as.integer(dfNp$D11_m == "Sempre o q") +
    1*as.integer(dfNp$D11_n == "Mai o quas") + 2*as.integer(dfNp$D11_n == "Qualche vo") + 3*as.integer(dfNp$D11_n == "Spesso") + 4*as.integer(dfNp$D11_n == "Sempre o q")
) / 14.0

# Debiasing
dfNp$debiased_a_score_presidi = (
  1*as.integer(dfNp$D11_a == "Mai o quas") + 2*as.integer(dfNp$D11_a == "Qualche vo") + 3*as.integer(dfNp$D11_a == "Spesso") + 4*as.integer(dfNp$D11_a == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_b_score_presidi = (
  1*as.integer(dfNp$D11_b == "Mai o quas") + 2*as.integer(dfNp$D11_b == "Qualche vo") + 3*as.integer(dfNp$D11_b == "Spesso") + 4*as.integer(dfNp$D11_b == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_c_score_presidi = (
  1*as.integer(dfNp$D11_c == "Mai o quas") + 2*as.integer(dfNp$D11_c == "Qualche vo") + 3*as.integer(dfNp$D11_c == "Spesso") + 4*as.integer(dfNp$D11_c == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_d_score_presidi = (
  1*as.integer(dfNp$D11_d == "Mai o quas") + 2*as.integer(dfNp$D11_d == "Qualche vo") + 3*as.integer(dfNp$D11_d == "Spesso") + 4*as.integer(dfNp$D11_d == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_e_score_presidi = (
  1*as.integer(dfNp$D11_e == "Mai o quas") + 2*as.integer(dfNp$D11_e == "Qualche vo") + 3*as.integer(dfNp$D11_e == "Spesso") + 4*as.integer(dfNp$D11_e == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_f_score_presidi = (
  1*as.integer(dfNp$D11_f == "Mai o quas") + 2*as.integer(dfNp$D11_f == "Qualche vo") + 3*as.integer(dfNp$D11_f == "Spesso") + 4*as.integer(dfNp$D11_f == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_g_score_presidi = (
  1*as.integer(dfNp$D11_g == "Mai o quas") + 2*as.integer(dfNp$D11_g == "Qualche vo") + 3*as.integer(dfNp$D11_g == "Spesso") + 4*as.integer(dfNp$D11_g == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_h_score_presidi = (
  1*as.integer(dfNp$D11_h == "Mai o quas") + 2*as.integer(dfNp$D11_h == "Qualche vo") + 3*as.integer(dfNp$D11_h == "Spesso") + 4*as.integer(dfNp$D11_h == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_i_score_presidi = (
  1*as.integer(dfNp$D11_i == "Mai o quas") + 2*as.integer(dfNp$D11_i == "Qualche vo") + 3*as.integer(dfNp$D11_i == "Spesso") + 4*as.integer(dfNp$D11_i == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_j_score_presidi = (
  1*as.integer(dfNp$D11_j == "Mai o quas") + 2*as.integer(dfNp$D11_j == "Qualche vo") + 3*as.integer(dfNp$D11_j == "Spesso") + 4*as.integer(dfNp$D11_j == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_k_score_presidi = (
  1*as.integer(dfNp$D11_k == "Mai o quas") + 2*as.integer(dfNp$D11_k == "Qualche vo") + 3*as.integer(dfNp$D11_k == "Spesso") + 4*as.integer(dfNp$D11_k == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_l_score_presidi = (
  1*as.integer(dfNp$D11_l == "Mai o quas") + 2*as.integer(dfNp$D11_l == "Qualche vo") + 3*as.integer(dfNp$D11_l == "Spesso") + 4*as.integer(dfNp$D11_l == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_m_score_presidi = (
  1*as.integer(dfNp$D11_m == "Mai o quas") + 2*as.integer(dfNp$D11_m == "Qualche vo") + 3*as.integer(dfNp$D11_m == "Spesso") + 4*as.integer(dfNp$D11_m == "Sempre o q")
) - dfNp$bias_score_presidi
dfNp$debiased_n_score_presidi = (
  1*as.integer(dfNp$D11_n == "Mai o quas") + 2*as.integer(dfNp$D11_n == "Qualche vo") + 3*as.integer(dfNp$D11_n == "Spesso") + 4*as.integer(dfNp$D11_n == "Sempre o q")
) - dfNp$bias_score_presidi


hist(dfNp$debiased_a_score_presidi)



# # # # # # Debiasing indici presidi : TUTTA ITALIA

dfp$bias_score_presidi = (
  1*as.integer(dfp$D11_a == "Mai o quas") + 2*as.integer(dfp$D11_a == "Qualche vo") + 3*as.integer(dfp$D11_a == "Spesso") + 4*as.integer(dfp$D11_a == "Sempre o q") +
    1*as.integer(dfp$D11_b == "Mai o quas") + 2*as.integer(dfp$D11_b == "Qualche vo") + 3*as.integer(dfp$D11_b == "Spesso") + 4*as.integer(dfp$D11_b == "Sempre o q") +
    1*as.integer(dfp$D11_c == "Mai o quas") + 2*as.integer(dfp$D11_c == "Qualche vo") + 3*as.integer(dfp$D11_c == "Spesso") + 4*as.integer(dfp$D11_c == "Sempre o q") +
    1*as.integer(dfp$D11_d == "Mai o quas") + 2*as.integer(dfp$D11_d == "Qualche vo") + 3*as.integer(dfp$D11_d == "Spesso") + 4*as.integer(dfp$D11_d == "Sempre o q") +
    1*as.integer(dfp$D11_e == "Mai o quas") + 2*as.integer(dfp$D11_e == "Qualche vo") + 3*as.integer(dfp$D11_e == "Spesso") + 4*as.integer(dfp$D11_e == "Sempre o q") +
    1*as.integer(dfp$D11_f == "Mai o quas") + 2*as.integer(dfp$D11_f == "Qualche vo") + 3*as.integer(dfp$D11_f == "Spesso") + 4*as.integer(dfp$D11_f == "Sempre o q") +
    1*as.integer(dfp$D11_g == "Mai o quas") + 2*as.integer(dfp$D11_g == "Qualche vo") + 3*as.integer(dfp$D11_g == "Spesso") + 4*as.integer(dfp$D11_g == "Sempre o q") +
    1*as.integer(dfp$D11_h == "Mai o quas") + 2*as.integer(dfp$D11_h == "Qualche vo") + 3*as.integer(dfp$D11_h == "Spesso") + 4*as.integer(dfp$D11_h == "Sempre o q") +
    1*as.integer(dfp$D11_i == "Mai o quas") + 2*as.integer(dfp$D11_i == "Qualche vo") + 3*as.integer(dfp$D11_i == "Spesso") + 4*as.integer(dfp$D11_i == "Sempre o q") +
    1*as.integer(dfp$D11_j == "Mai o quas") + 2*as.integer(dfp$D11_j == "Qualche vo") + 3*as.integer(dfp$D11_j == "Spesso") + 4*as.integer(dfp$D11_j == "Sempre o q") +
    1*as.integer(dfp$D11_k == "Mai o quas") + 2*as.integer(dfp$D11_k == "Qualche vo") + 3*as.integer(dfp$D11_k == "Spesso") + 4*as.integer(dfp$D11_k == "Sempre o q") +
    1*as.integer(dfp$D11_l == "Mai o quas") + 2*as.integer(dfp$D11_l == "Qualche vo") + 3*as.integer(dfp$D11_l == "Spesso") + 4*as.integer(dfp$D11_l == "Sempre o q") +
    1*as.integer(dfp$D11_m == "Mai o quas") + 2*as.integer(dfp$D11_m == "Qualche vo") + 3*as.integer(dfp$D11_m == "Spesso") + 4*as.integer(dfp$D11_m == "Sempre o q") +
    1*as.integer(dfp$D11_n == "Mai o quas") + 2*as.integer(dfp$D11_n == "Qualche vo") + 3*as.integer(dfp$D11_n == "Spesso") + 4*as.integer(dfp$D11_n == "Sempre o q")
) / 14.0

# Debiasing
dfp$debiased_a_score_presidi = (
  1*as.integer(dfp$D11_a == "Mai o quas") + 2*as.integer(dfp$D11_a == "Qualche vo") + 3*as.integer(dfp$D11_a == "Spesso") + 4*as.integer(dfp$D11_a == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_b_score_presidi = (
  1*as.integer(dfp$D11_b == "Mai o quas") + 2*as.integer(dfp$D11_b == "Qualche vo") + 3*as.integer(dfp$D11_b == "Spesso") + 4*as.integer(dfp$D11_b == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_c_score_presidi = (
  1*as.integer(dfp$D11_c == "Mai o quas") + 2*as.integer(dfp$D11_c == "Qualche vo") + 3*as.integer(dfp$D11_c == "Spesso") + 4*as.integer(dfp$D11_c == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_d_score_presidi = (
  1*as.integer(dfp$D11_d == "Mai o quas") + 2*as.integer(dfp$D11_d == "Qualche vo") + 3*as.integer(dfp$D11_d == "Spesso") + 4*as.integer(dfp$D11_d == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_e_score_presidi = (
  1*as.integer(dfp$D11_e == "Mai o quas") + 2*as.integer(dfp$D11_e == "Qualche vo") + 3*as.integer(dfp$D11_e == "Spesso") + 4*as.integer(dfp$D11_e == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_f_score_presidi = (
  1*as.integer(dfp$D11_f == "Mai o quas") + 2*as.integer(dfp$D11_f == "Qualche vo") + 3*as.integer(dfp$D11_f == "Spesso") + 4*as.integer(dfp$D11_f == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_g_score_presidi = (
  1*as.integer(dfp$D11_g == "Mai o quas") + 2*as.integer(dfp$D11_g == "Qualche vo") + 3*as.integer(dfp$D11_g == "Spesso") + 4*as.integer(dfp$D11_g == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_h_score_presidi = (
  1*as.integer(dfp$D11_h == "Mai o quas") + 2*as.integer(dfp$D11_h == "Qualche vo") + 3*as.integer(dfp$D11_h == "Spesso") + 4*as.integer(dfp$D11_h == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_i_score_presidi = (
  1*as.integer(dfp$D11_i == "Mai o quas") + 2*as.integer(dfp$D11_i == "Qualche vo") + 3*as.integer(dfp$D11_i == "Spesso") + 4*as.integer(dfp$D11_i == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_j_score_presidi = (
  1*as.integer(dfp$D11_j == "Mai o quas") + 2*as.integer(dfp$D11_j == "Qualche vo") + 3*as.integer(dfp$D11_j == "Spesso") + 4*as.integer(dfp$D11_j == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_k_score_presidi = (
  1*as.integer(dfp$D11_k == "Mai o quas") + 2*as.integer(dfp$D11_k == "Qualche vo") + 3*as.integer(dfp$D11_k == "Spesso") + 4*as.integer(dfp$D11_k == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_l_score_presidi = (
  1*as.integer(dfp$D11_l == "Mai o quas") + 2*as.integer(dfp$D11_l == "Qualche vo") + 3*as.integer(dfp$D11_l == "Spesso") + 4*as.integer(dfp$D11_l == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_m_score_presidi = (
  1*as.integer(dfp$D11_m == "Mai o quas") + 2*as.integer(dfp$D11_m == "Qualche vo") + 3*as.integer(dfp$D11_m == "Spesso") + 4*as.integer(dfp$D11_m == "Sempre o q")
) - dfp$bias_score_presidi
dfp$debiased_n_score_presidi = (
  1*as.integer(dfp$D11_n == "Mai o quas") + 2*as.integer(dfp$D11_n == "Qualche vo") + 3*as.integer(dfp$D11_n == "Spesso") + 4*as.integer(dfp$D11_n == "Sempre o q")
) - dfp$bias_score_presidi


hist(dfp$debiased_a_score_presidi)


df$opinione_associazioni_genitori_binary = as.integer(df$opinione_associazioni_genitori > 0)
df$strumenti_binary = as.integer(df$strumenti > 1.1)

dfC = df[df$Centro == 1,]
dfS = df[df$Sud == 1,]
dfNC = df[df$Sud != 1,]


df$ita8_plus_mate8_mean = df$mate8_mean + df$ita8_mean

df$opinione_associazioni_genitori_binary = as.integer(df$opinione_associazioni_genitori > 0)

dfN$opinione_associazioni_genitori_binary = as.integer(dfN$opinione_associazioni_genitori > 0)
dfC$opinione_associazioni_genitori_binary = as.integer(dfC$opinione_associazioni_genitori > 0)
dfS$opinione_associazioni_genitori_binary = as.integer(dfS$opinione_associazioni_genitori > 0)
dfNC$opinione_associazioni_genitori_binary = as.integer(dfNC$opinione_associazioni_genitori > 0)


df$strumenti_binary = as.integer(df$strumenti > 1.1)


# Now, debiasing dataframes for Centro, Sud e Centro Nord
dfCp = merge(x = dfC, y = grades, by = "cod_scu_anonimo", all.x = TRUE, suffixes = c("",".y"))
dfSp = merge(x = dfS, y = grades, by = "cod_scu_anonimo", all.x = TRUE, suffixes = c("",".y"))
dfNCp = merge(x = dfNC, y = grades, by = "cod_scu_anonimo", all.x = TRUE, suffixes = c("",".y"))

dfCp$bias_score_presidi = (
  1*as.integer(dfCp$D11_a == "Mai o quas") + 2*as.integer(dfCp$D11_a == "Qualche vo") + 3*as.integer(dfCp$D11_a == "Spesso") + 4*as.integer(dfCp$D11_a == "Sempre o q") +
    1*as.integer(dfCp$D11_b == "Mai o quas") + 2*as.integer(dfCp$D11_b == "Qualche vo") + 3*as.integer(dfCp$D11_b == "Spesso") + 4*as.integer(dfCp$D11_b == "Sempre o q") +
    1*as.integer(dfCp$D11_c == "Mai o quas") + 2*as.integer(dfCp$D11_c == "Qualche vo") + 3*as.integer(dfCp$D11_c == "Spesso") + 4*as.integer(dfCp$D11_c == "Sempre o q") +
    1*as.integer(dfCp$D11_d == "Mai o quas") + 2*as.integer(dfCp$D11_d == "Qualche vo") + 3*as.integer(dfCp$D11_d == "Spesso") + 4*as.integer(dfCp$D11_d == "Sempre o q") +
    1*as.integer(dfCp$D11_e == "Mai o quas") + 2*as.integer(dfCp$D11_e == "Qualche vo") + 3*as.integer(dfCp$D11_e == "Spesso") + 4*as.integer(dfCp$D11_e == "Sempre o q") +
    1*as.integer(dfCp$D11_f == "Mai o quas") + 2*as.integer(dfCp$D11_f == "Qualche vo") + 3*as.integer(dfCp$D11_f == "Spesso") + 4*as.integer(dfCp$D11_f == "Sempre o q") +
    1*as.integer(dfCp$D11_g == "Mai o quas") + 2*as.integer(dfCp$D11_g == "Qualche vo") + 3*as.integer(dfCp$D11_g == "Spesso") + 4*as.integer(dfCp$D11_g == "Sempre o q") +
    1*as.integer(dfCp$D11_h == "Mai o quas") + 2*as.integer(dfCp$D11_h == "Qualche vo") + 3*as.integer(dfCp$D11_h == "Spesso") + 4*as.integer(dfCp$D11_h == "Sempre o q") +
    1*as.integer(dfCp$D11_i == "Mai o quas") + 2*as.integer(dfCp$D11_i == "Qualche vo") + 3*as.integer(dfCp$D11_i == "Spesso") + 4*as.integer(dfCp$D11_i == "Sempre o q") +
    1*as.integer(dfCp$D11_j == "Mai o quas") + 2*as.integer(dfCp$D11_j == "Qualche vo") + 3*as.integer(dfCp$D11_j == "Spesso") + 4*as.integer(dfCp$D11_j == "Sempre o q") +
    1*as.integer(dfCp$D11_k == "Mai o quas") + 2*as.integer(dfCp$D11_k == "Qualche vo") + 3*as.integer(dfCp$D11_k == "Spesso") + 4*as.integer(dfCp$D11_k == "Sempre o q") +
    1*as.integer(dfCp$D11_l == "Mai o quas") + 2*as.integer(dfCp$D11_l == "Qualche vo") + 3*as.integer(dfCp$D11_l == "Spesso") + 4*as.integer(dfCp$D11_l == "Sempre o q") +
    1*as.integer(dfCp$D11_m == "Mai o quas") + 2*as.integer(dfCp$D11_m == "Qualche vo") + 3*as.integer(dfCp$D11_m == "Spesso") + 4*as.integer(dfCp$D11_m == "Sempre o q") +
    1*as.integer(dfCp$D11_n == "Mai o quas") + 2*as.integer(dfCp$D11_n == "Qualche vo") + 3*as.integer(dfCp$D11_n == "Spesso") + 4*as.integer(dfCp$D11_n == "Sempre o q")
) / 14.0

# Debiasing
dfCp$debiased_a_score_presidi = (
  1*as.integer(dfCp$D11_a == "Mai o quas") + 2*as.integer(dfCp$D11_a == "Qualche vo") + 3*as.integer(dfCp$D11_a == "Spesso") + 4*as.integer(dfCp$D11_a == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_b_score_presidi = (
  1*as.integer(dfCp$D11_b == "Mai o quas") + 2*as.integer(dfCp$D11_b == "Qualche vo") + 3*as.integer(dfCp$D11_b == "Spesso") + 4*as.integer(dfCp$D11_b == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_c_score_presidi = (
  1*as.integer(dfCp$D11_c == "Mai o quas") + 2*as.integer(dfCp$D11_c == "Qualche vo") + 3*as.integer(dfCp$D11_c == "Spesso") + 4*as.integer(dfCp$D11_c == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_d_score_presidi = (
  1*as.integer(dfCp$D11_d == "Mai o quas") + 2*as.integer(dfCp$D11_d == "Qualche vo") + 3*as.integer(dfCp$D11_d == "Spesso") + 4*as.integer(dfCp$D11_d == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_e_score_presidi = (
  1*as.integer(dfCp$D11_e == "Mai o quas") + 2*as.integer(dfCp$D11_e == "Qualche vo") + 3*as.integer(dfCp$D11_e == "Spesso") + 4*as.integer(dfCp$D11_e == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_f_score_presidi = (
  1*as.integer(dfCp$D11_f == "Mai o quas") + 2*as.integer(dfCp$D11_f == "Qualche vo") + 3*as.integer(dfCp$D11_f == "Spesso") + 4*as.integer(dfCp$D11_f == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_g_score_presidi = (
  1*as.integer(dfCp$D11_g == "Mai o quas") + 2*as.integer(dfCp$D11_g == "Qualche vo") + 3*as.integer(dfCp$D11_g == "Spesso") + 4*as.integer(dfCp$D11_g == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_h_score_presidi = (
  1*as.integer(dfCp$D11_h == "Mai o quas") + 2*as.integer(dfCp$D11_h == "Qualche vo") + 3*as.integer(dfCp$D11_h == "Spesso") + 4*as.integer(dfCp$D11_h == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_i_score_presidi = (
  1*as.integer(dfCp$D11_i == "Mai o quas") + 2*as.integer(dfCp$D11_i == "Qualche vo") + 3*as.integer(dfCp$D11_i == "Spesso") + 4*as.integer(dfCp$D11_i == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_j_score_presidi = (
  1*as.integer(dfCp$D11_j == "Mai o quas") + 2*as.integer(dfCp$D11_j == "Qualche vo") + 3*as.integer(dfCp$D11_j == "Spesso") + 4*as.integer(dfCp$D11_j == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_k_score_presidi = (
  1*as.integer(dfCp$D11_k == "Mai o quas") + 2*as.integer(dfCp$D11_k == "Qualche vo") + 3*as.integer(dfCp$D11_k == "Spesso") + 4*as.integer(dfCp$D11_k == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_l_score_presidi = (
  1*as.integer(dfCp$D11_l == "Mai o quas") + 2*as.integer(dfCp$D11_l == "Qualche vo") + 3*as.integer(dfCp$D11_l == "Spesso") + 4*as.integer(dfCp$D11_l == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_m_score_presidi = (
  1*as.integer(dfCp$D11_m == "Mai o quas") + 2*as.integer(dfCp$D11_m == "Qualche vo") + 3*as.integer(dfCp$D11_m == "Spesso") + 4*as.integer(dfCp$D11_m == "Sempre o q")
) - dfCp$bias_score_presidi
dfCp$debiased_n_score_presidi = (
  1*as.integer(dfCp$D11_n == "Mai o quas") + 2*as.integer(dfCp$D11_n == "Qualche vo") + 3*as.integer(dfCp$D11_n == "Spesso") + 4*as.integer(dfCp$D11_n == "Sempre o q")
) - dfCp$bias_score_presidi


hist(dfCp$debiased_a_score_presidi)






dfSp$bias_score_presidi = (
  1*as.integer(dfSp$D11_a == "Mai o quas") + 2*as.integer(dfSp$D11_a == "Qualche vo") + 3*as.integer(dfSp$D11_a == "Spesso") + 4*as.integer(dfSp$D11_a == "Sempre o q") +
    1*as.integer(dfSp$D11_b == "Mai o quas") + 2*as.integer(dfSp$D11_b == "Qualche vo") + 3*as.integer(dfSp$D11_b == "Spesso") + 4*as.integer(dfSp$D11_b == "Sempre o q") +
    1*as.integer(dfSp$D11_c == "Mai o quas") + 2*as.integer(dfSp$D11_c == "Qualche vo") + 3*as.integer(dfSp$D11_c == "Spesso") + 4*as.integer(dfSp$D11_c == "Sempre o q") +
    1*as.integer(dfSp$D11_d == "Mai o quas") + 2*as.integer(dfSp$D11_d == "Qualche vo") + 3*as.integer(dfSp$D11_d == "Spesso") + 4*as.integer(dfSp$D11_d == "Sempre o q") +
    1*as.integer(dfSp$D11_e == "Mai o quas") + 2*as.integer(dfSp$D11_e == "Qualche vo") + 3*as.integer(dfSp$D11_e == "Spesso") + 4*as.integer(dfSp$D11_e == "Sempre o q") +
    1*as.integer(dfSp$D11_f == "Mai o quas") + 2*as.integer(dfSp$D11_f == "Qualche vo") + 3*as.integer(dfSp$D11_f == "Spesso") + 4*as.integer(dfSp$D11_f == "Sempre o q") +
    1*as.integer(dfSp$D11_g == "Mai o quas") + 2*as.integer(dfSp$D11_g == "Qualche vo") + 3*as.integer(dfSp$D11_g == "Spesso") + 4*as.integer(dfSp$D11_g == "Sempre o q") +
    1*as.integer(dfSp$D11_h == "Mai o quas") + 2*as.integer(dfSp$D11_h == "Qualche vo") + 3*as.integer(dfSp$D11_h == "Spesso") + 4*as.integer(dfSp$D11_h == "Sempre o q") +
    1*as.integer(dfSp$D11_i == "Mai o quas") + 2*as.integer(dfSp$D11_i == "Qualche vo") + 3*as.integer(dfSp$D11_i == "Spesso") + 4*as.integer(dfSp$D11_i == "Sempre o q") +
    1*as.integer(dfSp$D11_j == "Mai o quas") + 2*as.integer(dfSp$D11_j == "Qualche vo") + 3*as.integer(dfSp$D11_j == "Spesso") + 4*as.integer(dfSp$D11_j == "Sempre o q") +
    1*as.integer(dfSp$D11_k == "Mai o quas") + 2*as.integer(dfSp$D11_k == "Qualche vo") + 3*as.integer(dfSp$D11_k == "Spesso") + 4*as.integer(dfSp$D11_k == "Sempre o q") +
    1*as.integer(dfSp$D11_l == "Mai o quas") + 2*as.integer(dfSp$D11_l == "Qualche vo") + 3*as.integer(dfSp$D11_l == "Spesso") + 4*as.integer(dfSp$D11_l == "Sempre o q") +
    1*as.integer(dfSp$D11_m == "Mai o quas") + 2*as.integer(dfSp$D11_m == "Qualche vo") + 3*as.integer(dfSp$D11_m == "Spesso") + 4*as.integer(dfSp$D11_m == "Sempre o q") +
    1*as.integer(dfSp$D11_n == "Mai o quas") + 2*as.integer(dfSp$D11_n == "Qualche vo") + 3*as.integer(dfSp$D11_n == "Spesso") + 4*as.integer(dfSp$D11_n == "Sempre o q")
) / 14.0

# Debiasing
dfSp$debiased_a_score_presidi = (
  1*as.integer(dfSp$D11_a == "Mai o quas") + 2*as.integer(dfSp$D11_a == "Qualche vo") + 3*as.integer(dfSp$D11_a == "Spesso") + 4*as.integer(dfSp$D11_a == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_b_score_presidi = (
  1*as.integer(dfSp$D11_b == "Mai o quas") + 2*as.integer(dfSp$D11_b == "Qualche vo") + 3*as.integer(dfSp$D11_b == "Spesso") + 4*as.integer(dfSp$D11_b == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_c_score_presidi = (
  1*as.integer(dfSp$D11_c == "Mai o quas") + 2*as.integer(dfSp$D11_c == "Qualche vo") + 3*as.integer(dfSp$D11_c == "Spesso") + 4*as.integer(dfSp$D11_c == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_d_score_presidi = (
  1*as.integer(dfSp$D11_d == "Mai o quas") + 2*as.integer(dfSp$D11_d == "Qualche vo") + 3*as.integer(dfSp$D11_d == "Spesso") + 4*as.integer(dfSp$D11_d == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_e_score_presidi = (
  1*as.integer(dfSp$D11_e == "Mai o quas") + 2*as.integer(dfSp$D11_e == "Qualche vo") + 3*as.integer(dfSp$D11_e == "Spesso") + 4*as.integer(dfSp$D11_e == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_f_score_presidi = (
  1*as.integer(dfSp$D11_f == "Mai o quas") + 2*as.integer(dfSp$D11_f == "Qualche vo") + 3*as.integer(dfSp$D11_f == "Spesso") + 4*as.integer(dfSp$D11_f == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_g_score_presidi = (
  1*as.integer(dfSp$D11_g == "Mai o quas") + 2*as.integer(dfSp$D11_g == "Qualche vo") + 3*as.integer(dfSp$D11_g == "Spesso") + 4*as.integer(dfSp$D11_g == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_h_score_presidi = (
  1*as.integer(dfSp$D11_h == "Mai o quas") + 2*as.integer(dfSp$D11_h == "Qualche vo") + 3*as.integer(dfSp$D11_h == "Spesso") + 4*as.integer(dfSp$D11_h == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_i_score_presidi = (
  1*as.integer(dfSp$D11_i == "Mai o quas") + 2*as.integer(dfSp$D11_i == "Qualche vo") + 3*as.integer(dfSp$D11_i == "Spesso") + 4*as.integer(dfSp$D11_i == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_j_score_presidi = (
  1*as.integer(dfSp$D11_j == "Mai o quas") + 2*as.integer(dfSp$D11_j == "Qualche vo") + 3*as.integer(dfSp$D11_j == "Spesso") + 4*as.integer(dfSp$D11_j == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_k_score_presidi = (
  1*as.integer(dfSp$D11_k == "Mai o quas") + 2*as.integer(dfSp$D11_k == "Qualche vo") + 3*as.integer(dfSp$D11_k == "Spesso") + 4*as.integer(dfSp$D11_k == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_l_score_presidi = (
  1*as.integer(dfSp$D11_l == "Mai o quas") + 2*as.integer(dfSp$D11_l == "Qualche vo") + 3*as.integer(dfSp$D11_l == "Spesso") + 4*as.integer(dfSp$D11_l == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_m_score_presidi = (
  1*as.integer(dfSp$D11_m == "Mai o quas") + 2*as.integer(dfSp$D11_m == "Qualche vo") + 3*as.integer(dfSp$D11_m == "Spesso") + 4*as.integer(dfSp$D11_m == "Sempre o q")
) - dfSp$bias_score_presidi
dfSp$debiased_n_score_presidi = (
  1*as.integer(dfSp$D11_n == "Mai o quas") + 2*as.integer(dfSp$D11_n == "Qualche vo") + 3*as.integer(dfSp$D11_n == "Spesso") + 4*as.integer(dfSp$D11_n == "Sempre o q")
) - dfSp$bias_score_presidi


hist(dfSp$debiased_a_score_presidi)




dfNCp$bias_score_presidi = (
  1*as.integer(dfNCp$D11_a == "Mai o quas") + 2*as.integer(dfNCp$D11_a == "Qualche vo") + 3*as.integer(dfNCp$D11_a == "Spesso") + 4*as.integer(dfNCp$D11_a == "Sempre o q") +
    1*as.integer(dfNCp$D11_b == "Mai o quas") + 2*as.integer(dfNCp$D11_b == "Qualche vo") + 3*as.integer(dfNCp$D11_b == "Spesso") + 4*as.integer(dfNCp$D11_b == "Sempre o q") +
    1*as.integer(dfNCp$D11_c == "Mai o quas") + 2*as.integer(dfNCp$D11_c == "Qualche vo") + 3*as.integer(dfNCp$D11_c == "Spesso") + 4*as.integer(dfNCp$D11_c == "Sempre o q") +
    1*as.integer(dfNCp$D11_d == "Mai o quas") + 2*as.integer(dfNCp$D11_d == "Qualche vo") + 3*as.integer(dfNCp$D11_d == "Spesso") + 4*as.integer(dfNCp$D11_d == "Sempre o q") +
    1*as.integer(dfNCp$D11_e == "Mai o quas") + 2*as.integer(dfNCp$D11_e == "Qualche vo") + 3*as.integer(dfNCp$D11_e == "Spesso") + 4*as.integer(dfNCp$D11_e == "Sempre o q") +
    1*as.integer(dfNCp$D11_f == "Mai o quas") + 2*as.integer(dfNCp$D11_f == "Qualche vo") + 3*as.integer(dfNCp$D11_f == "Spesso") + 4*as.integer(dfNCp$D11_f == "Sempre o q") +
    1*as.integer(dfNCp$D11_g == "Mai o quas") + 2*as.integer(dfNCp$D11_g == "Qualche vo") + 3*as.integer(dfNCp$D11_g == "Spesso") + 4*as.integer(dfNCp$D11_g == "Sempre o q") +
    1*as.integer(dfNCp$D11_h == "Mai o quas") + 2*as.integer(dfNCp$D11_h == "Qualche vo") + 3*as.integer(dfNCp$D11_h == "Spesso") + 4*as.integer(dfNCp$D11_h == "Sempre o q") +
    1*as.integer(dfNCp$D11_i == "Mai o quas") + 2*as.integer(dfNCp$D11_i == "Qualche vo") + 3*as.integer(dfNCp$D11_i == "Spesso") + 4*as.integer(dfNCp$D11_i == "Sempre o q") +
    1*as.integer(dfNCp$D11_j == "Mai o quas") + 2*as.integer(dfNCp$D11_j == "Qualche vo") + 3*as.integer(dfNCp$D11_j == "Spesso") + 4*as.integer(dfNCp$D11_j == "Sempre o q") +
    1*as.integer(dfNCp$D11_k == "Mai o quas") + 2*as.integer(dfNCp$D11_k == "Qualche vo") + 3*as.integer(dfNCp$D11_k == "Spesso") + 4*as.integer(dfNCp$D11_k == "Sempre o q") +
    1*as.integer(dfNCp$D11_l == "Mai o quas") + 2*as.integer(dfNCp$D11_l == "Qualche vo") + 3*as.integer(dfNCp$D11_l == "Spesso") + 4*as.integer(dfNCp$D11_l == "Sempre o q") +
    1*as.integer(dfNCp$D11_m == "Mai o quas") + 2*as.integer(dfNCp$D11_m == "Qualche vo") + 3*as.integer(dfNCp$D11_m == "Spesso") + 4*as.integer(dfNCp$D11_m == "Sempre o q") +
    1*as.integer(dfNCp$D11_n == "Mai o quas") + 2*as.integer(dfNCp$D11_n == "Qualche vo") + 3*as.integer(dfNCp$D11_n == "Spesso") + 4*as.integer(dfNCp$D11_n == "Sempre o q")
) / 14.0

# Debiasing
dfNCp$debiased_a_score_presidi = (
  1*as.integer(dfNCp$D11_a == "Mai o quas") + 2*as.integer(dfNCp$D11_a == "Qualche vo") + 3*as.integer(dfNCp$D11_a == "Spesso") + 4*as.integer(dfNCp$D11_a == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_b_score_presidi = (
  1*as.integer(dfNCp$D11_b == "Mai o quas") + 2*as.integer(dfNCp$D11_b == "Qualche vo") + 3*as.integer(dfNCp$D11_b == "Spesso") + 4*as.integer(dfNCp$D11_b == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_c_score_presidi = (
  1*as.integer(dfNCp$D11_c == "Mai o quas") + 2*as.integer(dfNCp$D11_c == "Qualche vo") + 3*as.integer(dfNCp$D11_c == "Spesso") + 4*as.integer(dfNCp$D11_c == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_d_score_presidi = (
  1*as.integer(dfNCp$D11_d == "Mai o quas") + 2*as.integer(dfNCp$D11_d == "Qualche vo") + 3*as.integer(dfNCp$D11_d == "Spesso") + 4*as.integer(dfNCp$D11_d == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_e_score_presidi = (
  1*as.integer(dfNCp$D11_e == "Mai o quas") + 2*as.integer(dfNCp$D11_e == "Qualche vo") + 3*as.integer(dfNCp$D11_e == "Spesso") + 4*as.integer(dfNCp$D11_e == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_f_score_presidi = (
  1*as.integer(dfNCp$D11_f == "Mai o quas") + 2*as.integer(dfNCp$D11_f == "Qualche vo") + 3*as.integer(dfNCp$D11_f == "Spesso") + 4*as.integer(dfNCp$D11_f == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_g_score_presidi = (
  1*as.integer(dfNCp$D11_g == "Mai o quas") + 2*as.integer(dfNCp$D11_g == "Qualche vo") + 3*as.integer(dfNCp$D11_g == "Spesso") + 4*as.integer(dfNCp$D11_g == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_h_score_presidi = (
  1*as.integer(dfNCp$D11_h == "Mai o quas") + 2*as.integer(dfNCp$D11_h == "Qualche vo") + 3*as.integer(dfNCp$D11_h == "Spesso") + 4*as.integer(dfNCp$D11_h == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_i_score_presidi = (
  1*as.integer(dfNCp$D11_i == "Mai o quas") + 2*as.integer(dfNCp$D11_i == "Qualche vo") + 3*as.integer(dfNCp$D11_i == "Spesso") + 4*as.integer(dfNCp$D11_i == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_j_score_presidi = (
  1*as.integer(dfNCp$D11_j == "Mai o quas") + 2*as.integer(dfNCp$D11_j == "Qualche vo") + 3*as.integer(dfNCp$D11_j == "Spesso") + 4*as.integer(dfNCp$D11_j == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_k_score_presidi = (
  1*as.integer(dfNCp$D11_k == "Mai o quas") + 2*as.integer(dfNCp$D11_k == "Qualche vo") + 3*as.integer(dfNCp$D11_k == "Spesso") + 4*as.integer(dfNCp$D11_k == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_l_score_presidi = (
  1*as.integer(dfNCp$D11_l == "Mai o quas") + 2*as.integer(dfNCp$D11_l == "Qualche vo") + 3*as.integer(dfNCp$D11_l == "Spesso") + 4*as.integer(dfNCp$D11_l == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_m_score_presidi = (
  1*as.integer(dfNCp$D11_m == "Mai o quas") + 2*as.integer(dfNCp$D11_m == "Qualche vo") + 3*as.integer(dfNCp$D11_m == "Spesso") + 4*as.integer(dfNCp$D11_m == "Sempre o q")
) - dfNCp$bias_score_presidi
dfNCp$debiased_n_score_presidi = (
  1*as.integer(dfNCp$D11_n == "Mai o quas") + 2*as.integer(dfNCp$D11_n == "Qualche vo") + 3*as.integer(dfNCp$D11_n == "Spesso") + 4*as.integer(dfNCp$D11_n == "Sempre o q")
) - dfNCp$bias_score_presidi

hist(dfNCp$debiased_a_score_presidi)


# Features
# Data Driven
dfp$data_driven_principal = as.integer(dfp$D11_j == "Sempre o q")
dfNp$data_driven_principal = as.integer(dfNp$D11_j == "Sempre o q") #+ as.integer(dfNp$D11_j == "Spesso")

# Controllore
dfp$controllore_principal = as.integer(dfp$D11_f == "Sempre o q")
dfNp$controllore_principal = as.integer(dfNp$D11_f == "Sempre o q")# + as.integer(dfNp$D11_f == "Spesso")








