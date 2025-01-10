rm(list = ls())

library(QuantPsyc)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)

## Load and modify data ----------------------------------------

data.dir <- file.path("Data", "Tse_et_al")
data.Tse.16  <- read_excel(file.path(data.dir, "Tse_et_al_2016.xlsx"))
data.Tse.23  <- read_excel(file.path(data.dir, "Tse_et_al.xlsx"))
data.Chan.24 <- read_excel(file.path(data.dir, "Chan and Tse.xlsx"))
Tse.Yap.Chan <- read.csv(file.path(data.dir, "data_both.csv"))

## CLD -------------------------------------------------------------------------

data.Tse.23 <- data.Tse.23 %>%
  dplyr::rename(
    nam.Ntrials = Ntrials,
    nam.Acc     = Acc,
    nam.RT      = RT,
    nam.RT.SD   = RT_SD,
    nam.zRT     = zRT
  )

data.Tse.16 <- data.Tse.16 %>%
  dplyr::rename(
    lex.Ntrials = Ntrials,
    lex.Acc     = Acc,
    lex.RT      = RT,
    lex.RT.SE   = `RT-SE`,
    lex.RT.SD   = `RT-SD`,
    lex.zRT     = zRT
  )

Tse_data <- merge(
  x = dplyr::select(data.Tse.16, Word_Trad:lex.zRT),
  y = data.Tse.23,
  by = c("Word_Trad", "Word_Sim")
)

CLP_data <- merge( # the Chinese Lexicon Project
  x = Tse_data,
  y = data.Chan.24,
  by = c("Word_Trad", "Word_Sim")
)

CLP_data <- CLP_data %>%
  dplyr::rename(
    Pron_1 = Pronunciation_1, 
    Pron_2 = Pronunciation_2, 
    IPF_1  = Initial_phoneme_feature_1, 
    IPF_2  = Initial_phoneme_feature_2, 
    IPF_3  = Initial_phoneme_feature_3, 
    IPF_4  = Initial_phoneme_feature_4, 
    IPF_5  = Initial_phoneme_feature_5, 
    IPF_6  = Initial_phoneme_feature_6, 
    IPF_7  = Initial_phoneme_feature_7, 
    IPF_8  = Initial_phoneme_feature_8, 
    IPF_9  = Initial_phoneme_feature_9, 
    IPF_10 = Initial_phoneme_feature_10, 
    IPF_11 = Initial_phoneme_feature_11, 
    IPF_12 = Initial_phoneme_feature_12, 
    IPF_13 = Initial_phoneme_feature_13, 
    NS_1   = Stroke_1, 
    NS_2   = Stroke_2, 
    Phon_1 = Phoneme_1, 
    Phon_2 = Phoneme_2, 
    Freq_C1   = Character_frequency_1, 
    Freq_C2   = Character_frequency_2, 
    Freq_word = Word_frequency, 
    REG.1_C1  = Regularity_1_dummy_contrast_1, 
    REG.1_C2  = Regularity_2_dummy_contrast_1, 
    REG.2_C1  = Regularity_1_dummy_contrast_2, 
    REG.2_C2  = Regularity_2_dummy_contrast_2, 
    P.cons_1  = Phono_Consistency_1, 
    P.cons_2  = Phono_Consistency_2, 
    OP.cons_1 = OP_consistency_1, 
    OP.cons_2 = OP_consistency_2, 
    HD_1    = Homophone_1,    # Homophone density
    HD_2    = Homophone_2, 
    NbS_1   = Neighborhood_1, # Neighborhood size
    NbS_2   = Neighborhood_2, 
    NMe_1   = Meaning_1,      # Number of meaning
    NMe_2   = Meaning_2, 
    Trans_1 = Transparency_1, # Semantic transparency
    Trans_2 = Transparency_2, 
    Vale = valence_mean, 
    Arou = arousal_mean, 
    Conc = conc_mean, 
    Fami = fami_mean, 
    Imag = imag_mean)

save(CLP_data, file = file.path(data.dir, "CLP_data.RData"))
write_xlsx(CLP_data, file.path(data.dir, "CLP_data.xlsx"))

## Tse.Yap.Chan ----------------------------------------------------------------

Tse.Yap.Chan$Task <- Tse.Yap.Chan$Task %>% 
  dplyr::recode(`0.5` = "LD", `-0.5` = "Naming")

Tse.Yap.Chan <- Tse.Yap.Chan %>% 
  dplyr::rename(
    C1_St.num = Char1_StrokeCount,  
    C2_St.num = Char2_StrokeCount, 
      # number of strokes (Que, 2008)
    C1_logF = logCHR1_CD_sim,  # CD: contextual diversity 
    C2_logF = logCHR2_CD_sim, 
    Word_logF = logW_CD_sim, 
    C_logF.sum = CHR_CD_sum, 
      # word and character (log) frequencies based on subtitle (Cai & Brysbaert, 2010; https://doi.org/10.1371/journal.pone.0010729)
    C1_Ph.num = Char1_PhonemeCount, 
    C2_Ph.num = Char2_PhonemeCount,
      # number of phoneme
    C1_Homo.D = Char1_homodena, 
    C2_Homo.D = Char2_homodena, 
      # homophone density
    C1_Ph.cons = Char1_phonocon, 
    C2_Ph.cons = Char2_phonocon, 
    C1_OP.cons = Char1_op_mapcon, 
    C2_OP.cons = Char2_op_mapcon, 
    C1_Reg.1 = Char1_PhonoReg1, 
    C1_Reg.2 = Char1_PhonoReg2, 
    C2_Reg.1 = Char2_PhonoReg1, 
    C2_Reg.2 = Char2_PhonoReg2, 
    C1_A_Nb.num = Char1_Nsize_A1, 
    C2_B_Nb.num = Char2_Nsize_B1, 
      # position specific neighborhood size
    C1_A_Nb.logF = Char1_NBlogFreq_A_mean, 
    C2_B_Nb.logF = Char2_NBlogFreq_B_mean, 
      # mean (log) word frequencies of all  position-specific neighbors
    C1_S.num = Char1_nummean, 
    C2_S.num = Char2_nummean, 
      # number of meaning
    C1_S.Trans = semrate1, 
    C2_S.Trans = semrate2, 
    C_S.T.avg = semrate, 
      # semantic transparency rating
    C1_S.T.Nb = ST_C1_NB_mean, 
    C2_S.T.Nb = ST_C2_NB_mean
      # mean semantic transparency between target word & all its neighbors
  )

data.Chan.24$Imag <- data.Chan.24$imag_mean

Tse.Yap.Chan <- merge(
  x = Tse.Yap.Chan, 
  y = dplyr::select(data.Chan.24, 
                    all_of(c("Word_Trad", "Imag"))), 
  by = "Word_Trad", 
  all.x = TRUE)

write.csv(Tse.Yap.Chan, 
          file = file.path(data.dir, "Tse_Yap_Chan_nz.csv"), 
          row.names = FALSE)

cols_to_z <- c(
  "C1_St.num", "C2_St.num", 
  "C1_logF", "C2_logF", "Word_logF", "C_logF.sum", 
  "C1_Ph.num", "C2_Ph.num", "C1_Homo.D", "C2_Homo.D", 
  "C1_Ph.cons", "C2_Ph.cons", "C1_OP.cons", "C2_OP.cons", 
  "C1_A_Nb.num", "C2_B_Nb.num", "C1_A_Nb.logF", "C2_B_Nb.logF", 
  "C1_S.num", "C2_S.num", "C1_S.Trans", "C2_S.Trans", 
  "C_S.T.avg", "C1_S.T.Nb", "C2_S.T.Nb", "Imag" 
)

Tse.Yap.Chan <- Tse.Yap.Chan %>%
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ QuantPsyc::Make.Z(.x),
    .names = "{.col}.Z"))

Tse.Yap.Chan <- Tse.Yap.Chan %>% 
  dplyr::select(-all_of(cols_to_z))

Tse.Yap.Chan <- Tse.Yap.Chan %>% 
  dplyr::select(-all_of(paste0("char1_", 1:13)))

save(Tse.Yap.Chan, 
     file = file.path(data.dir, "Tse_Yap_Chan.RData"))
write.csv(Tse.Yap.Chan, 
          file = file.path(data.dir, "Tse_Yap_Chan.csv"), 
          row.names = FALSE)


